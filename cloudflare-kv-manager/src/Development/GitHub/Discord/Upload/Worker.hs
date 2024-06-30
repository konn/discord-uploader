{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Development.GitHub.Discord.Upload.Worker (
  handlers,
  JSHandlers,
  JSObject (..),
) where

import Control.Exception.Safe (throwString)
import qualified Data.Aeson as J
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Time (runClock)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.KV
import qualified Network.Cloudflare.Worker.Binding.KV as KV
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch
import Network.Cloudflare.Worker.KVManager.Types (KVManager (..))
import Steward.Workers

type DiscoKVEnv = BindingsClass '["CF_TEAM_NAME"] '["CF_AUD_TAG"] '[ '("KV", KVClass)]

type KVFetcher = FetchHandler DiscoKVEnv

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch = kvWorker}

kvWorker :: KVFetcher
kvWorker = runWorker $ runClock do
  env <- getWorkerEnv @DiscoKVEnv
  let !rawTeam = getEnv "CF_TEAM_NAME" env
  teamName <- case J.fromJSON rawTeam of
    J.Error e ->
      throwString $
        "Could not parse CF_TEAM_NAME (" <> show rawTeam <> "): " <> e
    J.Success x -> pure x
  let !appAudienceID = T.pack $ getSecret "CF_AUD_TAG" env
      !authCfg = CloudflareTunnelConfig {..}
  withCloudflareTunnelAuth @DiscoKVEnv authCfg (const $ pure True) $
    const $
      fromHandlers @DiscoKVEnv apiEndpoints

apiEndpoints :: (Worker DiscoKVEnv :> es) => KVManager (Handler (Eff es))
apiEndpoints =
  KVManager
    { put = Handler handlePut
    , listKeys = Handler handleListKeys
    , get = Handler handleGet
    , delete = Handler handleDelete
    }

withKV :: (Worker DiscoKVEnv :> es) => (KV -> IO a) -> Eff es a
withKV f = do
  !kv <- getBinding "KV" <$> getWorkerEnv @DiscoKVEnv
  unsafeEff_ $ f kv

handleDelete :: (Worker DiscoKVEnv :> es) => T.Text -> Eff es ()
handleDelete key = withKV $ \kv -> KV.delete kv $ T.unpack key

handleGet :: (Worker DiscoKVEnv :> es) => T.Text -> Eff es (Maybe ValueWithMetadata)
handleGet key = withKV \kv ->
  KV.get kv (T.unpack key) <&> fmap \src ->
    fromMaybe ValueWithMetadata {value = src, metadata = Nothing} $ J.decode $ LTE.encodeUtf8 $ LT.pack src

handleListKeys :: (Worker DiscoKVEnv :> es) => ListKeys -> Eff es ListKeyResult
handleListKeys opts =
  either throwString pure =<< withKV \kv -> KV.listKeys kv opts

handlePut :: (Worker DiscoKVEnv :> es) => PutOptions -> T.Text -> T.Text -> Eff es ()
handlePut opts k v = withKV \kv ->
  KV.put kv opts {KV.metadata = Nothing} (T.unpack k) $
    LT.unpack $
      LTE.decodeUtf8 $
        J.encode $
          ValueWithMetadata
            { value = T.unpack v
            , metadata = opts.metadata
            }
