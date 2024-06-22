{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.GitHub.Discord.Upload.Worker (handlers, JSHandlers, JSObject (..)) where

import qualified Data.Aeson as J
import qualified Data.Text as T
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.Headers (js_fun_get_ByteString_nullable_ByteString)
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.KV
import qualified Network.Cloudflare.Worker.Binding.KV as KV
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch
import qualified Network.Cloudflare.Worker.Request as Req
import Network.Cloudflare.Worker.Response (newResponse)
import qualified Network.Cloudflare.Worker.Response as Resp

type DiscoKVEnvClass = BindingsClass '["CF_TEAM_NAME"] '["CF_AUD_TAG"] '[ '("KV", KVClass)]

type KVFetcher = FetchHandler DiscoKVEnvClass

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch = kvWorker}

kvWorker :: KVFetcher
kvWorker req env _ctx = do
  let hdrs = Req.getHeaders req
  cfAuth <-
    traverse toHaskellByteString . fromNullable
      =<< js_fun_get_ByteString_nullable_ByteString hdrs
      =<< fromHaskellByteString "Cf-Access-Jwt-Assertion"
  case cfAuth of
    Nothing -> do
      newResponse
        Resp.SimpleResponseInit
          { Resp.statusText = "Unauthorized"
          , Resp.status = 401
          , Resp.body = "Missing Cf-Access-Jwt-Assertion header"
          , Resp.headers = mempty
          }
    Just _ -> do
      case J.fromJSON $ getEnv "CF_TEAM_NAME" env of
        J.Error e -> do
          newResponse
            Resp.SimpleResponseInit
              { Resp.statusText = "Internal Server Error"
              , Resp.status = 500
              , Resp.body = "Failed to decode CF_TEAM_NAME: " <> T.pack e
              , Resp.headers = mempty
              }
        J.Success team -> do
          let kv = getBinding "KV" env
              audTag = getSecret "CF_AUD_TAG" env
              certUrl = "https://" <> team <> ".cloudflareaccess.com/cdn-cgi/access/certs"
          resp <- js_fetch $ toJSString certUrl
          mb <- fromNullable <$> Resp.getBody resp
          case mb of
            Nothing -> pure ()
            Just {} -> pure ()
          -- TODO: verify here
          keys <- KV.listKeys kv KV.ListKeys {limit = Nothing, prefix = Nothing, cursor = Nothing}
          newResponse
            Resp.SimpleResponseInit
              { Resp.statusText = "OK"
              , Resp.status = 200
              , Resp.body = "Keys traversed: " <> T.pack (show keys)
              , Resp.headers = mempty
              }

foreign import javascript unsafe "fetch($1)"
  js_fetch :: JSString -> IO Resp.WorkerResponse
