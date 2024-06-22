{-# LANGUAGE NoMonomorphismRestriction #-}

module Effectful.Network.Cloudflare.Workers.KV (
  KV,
  KVConfig (..),
  runKV,
  ListKeysOptions (..),
  KVResponse (..),
  KVError (..),
  Key (..),
  KeyEntry (..),
  KVMessage (..),
  ResultInfo (..),
  NamespaceID (..),

  -- * API
  listKeys,
  KeyInfo (..),
  deleteKeys,
  putKeyValues,
  KeyValuePair (..),
  getKeyMetadata,
  deleteKey,
  getKeyValue,
  putKeyValue,
) where

import Control.Exception.Safe (throwString)
import Control.Lens ((<&>))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Functor (void)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Network.Http
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.HTTP.Types (status404)
import Network.HTTP.Types.URI

data KV :: Effect where
  DeleteKeys :: NamespaceID -> [Key] -> KV m (KVResponse ())
  ListKeys :: NamespaceID -> ListKeysOptions -> KV m (KVResponse [KeyInfo])
  PutKeyValues :: (ToJSON a) => NamespaceID -> [KeyValuePair a] -> KV m (KVResponse ())
  GetKeyMetadata :: (FromJSON a) => NamespaceID -> Key -> KV m (KVResponse a)
  DeleteKey :: NamespaceID -> Key -> KV m (KVResponse ())
  GetKeyValue :: (FromJSON a) => NamespaceID -> Key -> KV m (Maybe (KeyEntry a))
  PutKeyValue :: (ToJSON a) => NamespaceID -> Key -> Maybe a -> T.Text -> KV m (KVResponse ())

type instance DispatchOf KV = Dynamic

listKeys :: (KV :> es) => NamespaceID -> ListKeysOptions -> Eff es (KVResponse [KeyInfo])
listKeys = fmap send . ListKeys

deleteKeys :: (KV :> es) => NamespaceID -> [Key] -> Eff es (KVResponse ())
deleteKeys = fmap send . DeleteKeys

getKeyMetadata ::
  (FromJSON a, KV :> es) =>
  NamespaceID ->
  Key ->
  Eff es (KVResponse a)
getKeyMetadata = fmap send . GetKeyMetadata

getKeyValue :: (FromJSON a, KV :> es) => NamespaceID -> Key -> Eff es (Maybe (KeyEntry a))
getKeyValue = fmap send . GetKeyValue

putKeyValue ::
  (ToJSON a, KV :> es) =>
  NamespaceID ->
  Key ->
  Maybe a ->
  T.Text ->
  Eff es (KVResponse ())
putKeyValue = fmap (fmap $ fmap send) . PutKeyValue

data KeyEntry a = KeyEntry {metadata :: !a, value :: !T.Text}
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data KeyValuePair a = KeyValuePair
  { base64 :: !(Maybe Bool)
  , expiration :: !(Maybe POSIXTime)
  , expiration_ttl :: !(Maybe Int)
  , key :: !T.Text
  , metadata :: !(Maybe a)
  , value :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, FromJSON, ToJSON)

putKeyValues ::
  (ToJSON a, KV :> es) =>
  NamespaceID ->
  [KeyValuePair a] ->
  Eff es (KVResponse ())
putKeyValues = fmap send . PutKeyValues

deleteKey :: (KV :> es) => NamespaceID -> Key -> Eff es (KVResponse ())
deleteKey = fmap send . DeleteKey

data KeyInfo = KeyInfo
  { expiration :: Maybe POSIXTime
  , metadata :: Maybe A.Value
  , name :: Key
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data KVResponse a = KVResponse
  { errors :: !(Maybe [KVError])
  , messages :: !(Maybe [KVMessage])
  , result :: !(Maybe a)
  , success :: !(Maybe Bool)
  , result_info :: !(Maybe ResultInfo)
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data ResultInfo = ResultInfo {count :: !Int, cursor :: !(Maybe Cursor)}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

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

newtype Cursor = Cursor {cursor :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (FromJSON, ToJSON)

data ListKeysOptions = ListKeysOptions
  { cursor :: !(Maybe Cursor)
  , limit :: !(Maybe Natural)
  , prefix :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Ord, Generic)

newtype Key = Key {key :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype NamespaceID = NamespaceID {rawID :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

newtype Utf8String = Utf8String {utf8String :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

instance IsString Utf8String where
  fromString = Utf8String . TE.encodeUtf8 . T.pack

instance FromJSON Utf8String where
  parseJSON = A.withText "Utf8String" $ pure . Utf8String . TE.encodeUtf8

instance ToJSON Utf8String where
  toJSON = A.String . TE.decodeUtf8 . (.utf8String)

instance FromJSONKey Utf8String where
  fromJSONKey = A.FromJSONKeyText $ Utf8String . TE.encodeUtf8

instance ToJSONKey Utf8String where
  toJSONKey = A.toJSONKeyText $ TE.decodeUtf8 . (.utf8String)

data KVConfig = KVConfig
  { accountId :: !String
  , apiKey :: !Utf8String
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

mkKVNamespaceRequest ::
  KVConfig ->
  NamespaceID ->
  String ->
  Request
mkKVNamespaceRequest cfg nm ep =
  let req0 =
        fromString $
          "https://api.cloudflare.com/client/v4/accounts/"
            <> cfg.accountId
            <> ("/storage/kv/namespaces/" <> T.unpack (TE.decodeUtf8 nm.rawID))
            <> "/"
            <> dropWhile (== '/') ep
   in req0
        { requestHeaders =
            ("Authorization", "Bearer " <> cfg.apiKey.utf8String)
              : req0.requestHeaders
        }

runKV :: (Http :> es) => KVConfig -> Eff (KV : es) a -> Eff es a
runKV cfg = interpret @KV \_localEs -> \case
  DeleteKeys ns keys -> do
    either throwString (pure . void @_ @A.Value)
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns "bulk")
          { method = "DELETE"
          , requestBody = RequestBodyLBS $ A.encode keys
          }
  PutKeyValues ns kvs -> do
    either throwString (pure . void @_ @A.Value)
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns "bulk")
          { method = "PUT"
          , requestBody = RequestBodyLBS $ A.encode kvs
          }
  ListKeys ns opts -> do
    either throwString pure
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns "keys")
          { method = "GET"
          , queryString =
              maybe "" (renderSimpleQuery True . NE.toList) $
                NE.nonEmpty $
                  catMaybes
                    [ ("cursor",) . TE.encodeUtf8 . (.cursor) <$> opts.cursor
                    , ("limit",) . BS8.pack . show <$> opts.limit
                    , ("prefix",) . TE.encodeUtf8 <$> opts.prefix
                    ]
          }
  GetKeyMetadata ns key -> do
    either throwString pure
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns $ "metadata/" <> T.unpack key.key)
          { method = "GET"
          }
  DeleteKey ns key -> do
    either throwString (pure . void @_ @A.Value)
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns $ "values/" <> T.unpack key.key) {method = "DELETE"}
  GetKeyValue ns key -> do
    mapM
      ( either throwString pure
          . A.eitherDecode
          . responseBody
      )
      =<< try404'
        ( httpLbs
            (mkKVNamespaceRequest cfg ns $ "values/" <> T.unpack key.key) {method = "GET"}
        )
  PutKeyValue ns key meta value -> do
    either throwString (pure . void @_ @A.Value)
      . A.eitherDecode
      . responseBody
      =<< httpLbs
        (mkKVNamespaceRequest cfg ns $ "values/" <> T.unpack key.key)
          { method = "PUT"
          , requestBody =
              RequestBodyLBS $
                A.encode $
                  A.object
                    [ "value" A..= value
                    , "metadata" A..= meta
                    ]
          }

try404' :: Eff es (Response a) -> Eff es (Maybe (Response a))
try404' act =
  try404 act
    <&> ( >>=
            \rsp ->
              if responseStatus rsp == status404
                then Nothing
                else Just rsp
        )
