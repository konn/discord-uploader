{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Effectful.Network.Cloudflare.Workers.KV (
  KV,
  runKV,
  KVError (..),
  KeyEntry (..),
  KVMessage (..),

  -- * API
  Effectful.Network.Cloudflare.Workers.KV.listKeys,
  ListKeys (..),
  listAllKeys,
  ListKeyResult (..),
  Key (..),
  deleteKey,
  getKeyValue,
  putKeyValue,
  PutOptions (..),
) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Lens ((.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Function ((&))
import Data.Functor.Of (Of)
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import Network.Cloudflare.Worker.KVManager.Types
import Steward.Client.Effectful (StewardClient)
import Steward.Types
import Streaming (lift)
import Streaming.Prelude qualified as S

data KV :: Effect where
  ListKeys_ :: ListKeys -> KV m ListKeyResult
  DeleteKey :: T.Text -> KV m ()
  GetKeyValue :: (FromJSON a) => T.Text -> KV m (Maybe (KeyEntry a))
  PutKeyValue :: (ToJSON a) => T.Text -> Maybe a -> T.Text -> KV m ()

type instance DispatchOf KV = Dynamic

listKeys :: (KV :> es) => ListKeys -> Eff es ListKeyResult
listKeys = send . ListKeys_

getKeyValue :: (FromJSON a, KV :> es) => T.Text -> Eff es (Maybe (KeyEntry a))
getKeyValue = send . GetKeyValue

putKeyValue ::
  (ToJSON a, KV :> es) =>
  T.Text ->
  Maybe a ->
  T.Text ->
  Eff es ()
putKeyValue = fmap (fmap send) . PutKeyValue

data KeyEntry a = KeyEntry {metadata :: !a, value :: !T.Text}
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, FromJSON, ToJSON)

deleteKey :: (KV :> es) => T.Text -> Eff es ()
deleteKey = send . DeleteKey

data KVError = KVError
  { code :: !Int
  , message :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data KVMessage = KVMessage
  { code :: !Int
  , message :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

effClient :: (StewardClient :> es) => KVManager (Client (Eff es))
effClient = client

runKV :: (StewardClient :> es) => Eff (KV : es) a -> Eff es a
runKV =
  interpret @KV \_localEs -> \case
    ListKeys_ opts -> effClient.listKeys.call opts
    DeleteKey key -> effClient.delete.call key
    GetKeyValue key -> do
      mapM decodeMeta =<< effClient.get.call key
    PutKeyValue key meta value -> do
      effClient.put.call
        PutOptions
          { expirationTtl = Nothing
          , expiration = Nothing
          , metadata = A.toJSON <$> meta
          }
        key
        value

decodeMeta ::
  forall a m.
  (FromJSON a, MonadThrow m) =>
  ValueWithMetadata ->
  m (KeyEntry a)
decodeMeta valMeta = do
  let value = T.pack valMeta.value
  metadata <- case A.fromJSON $ A.toJSON valMeta.metadata of
    A.Success a -> pure a
    A.Error err -> throwString err
  pure KeyEntry {..}

listAllKeys :: (KV :> es) => ListKeys -> S.Stream (Of Key) (Eff es) (Maybe Cursor)
listAllKeys = go
  where
    go !opts = do
      ListKeyResult {..} <-
        lift $
          Effectful.Network.Cloudflare.Workers.KV.listKeys opts
      S.each keys
      if list_complete
        then pure cursor
        else go $ opts & #cursor .~ cursor
