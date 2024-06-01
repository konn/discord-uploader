{-# LANGUAGE QuasiQuotes #-}

module Effectful.Network.Discord (
  DiscordConfig (..),
  DiscordBotToken (..),
  Nonce (..),
  ID (..),
  Timestamp (..),
  toUploadableAttachment,
  UploadableAttachment (..),

  -- * APIs
  createMessage,
  defaultCreateMessage,
  CreateMessage (..),
  editMessage,
  EditMessage (..),
  deleteMessage,

  -- * Utils
  callDiscordAPI,
  callDiscordAPIJSON,
  encodeAttachments,
  mkDiscordRequest,

  -- * Response types
  Message (..),
  Attachment (..),
  MessageComponent (..),
  MessageReference (..),
  Embed (..),
  AllowedMention (..),
  User (..),

  -- * Misc
  AvatarDecorationData (..),
  Poll (..),
  Sticker (..),
  Role (..),
  Application (..),
  Webhook (..),
  Channel (..),
  Reaction (..),
  MessageActivity (..),
  ChannelMention (..),
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (throwString)
import Control.Lens
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types (Value)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lens (packed)
import Data.Time (ZonedTime)
import Data.Time.Format.ISO8601 (ISO8601, iso8601ParseM, iso8601Show)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Http (Http, Request (..), RequestBody (..), Response (responseBody), httpLbs)
import GHC.Generics (Generic (..))
import Network.HTTP.Client.MultipartFormData qualified as MP
import Text.Read (readEither)

newtype DefaultJSON a = DefaultJSON {runDefaultJSON :: a}
  deriving (Show, Eq, Ord)

defOpts :: A.Options
defOpts =
  A.defaultOptions
    { A.fieldLabelModifier = packed %~ T.dropWhileEnd (== '_')
    , A.omitNothingFields = True
    }

instance
  ( Generic a
  , A.GToJSON' Value A.Zero (Rep a)
  ) =>
  ToJSON (DefaultJSON a)
  where
  toJSON = A.genericToJSON defOpts . (.runDefaultJSON)

instance
  ( Generic a
  , A.GFromJSON A.Zero (Rep a)
  ) =>
  FromJSON (DefaultJSON a)
  where
  parseJSON = fmap DefaultJSON . A.genericParseJSON defOpts

data Nonce = NonceText !T.Text | NonceInt !Int64
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Nonce where
  toJSON (NonceText t) = A.toJSON t
  toJSON (NonceInt i) = A.toJSON i

instance FromJSON Nonce where
  parseJSON obj =
    NonceText <$> A.parseJSON obj
      <|> NonceInt <$> A.parseJSON obj

data DiscordConfig = DiscordConfig
  { discordAppID :: !BS.ByteString
  , discordPubKey :: !BS.ByteString
  , discordToken :: !DiscordBotToken
  , discordChannel :: !(ID Channel)
  }
  deriving (Show, Eq, Ord, Generic)

newtype ID a = ID {id :: Int64}
  deriving (Generic)
  deriving (ToJSON, FromJSON) via Snowflake
  deriving newtype (Num, Eq, Ord, Show, Hashable, Read)

newtype Snowflake = Snowflake {rawId :: Int64}
  deriving (Generic)
  deriving newtype (Num, Eq, Ord, Show, Read, Hashable)

instance ToJSON Snowflake where
  toJSON = A.toJSON . show . (.rawId)

instance FromJSON Snowflake where
  parseJSON obj =
    Snowflake <$> A.parseJSON obj
      <|> Snowflake <$> A.withText "snowflake" (either fail pure . readEither . T.unpack) obj

data UploadableAttachment = UploadableAttachment
  { id :: !(ID Attachment)
  , filename :: !FilePath
  , description :: !(Maybe T.Text)
  , content_type :: !(Maybe T.Text)
  , body :: Maybe RequestBody
  }
  deriving (Generic)

instance Show UploadableAttachment where
  showsPrec _ attch =
    showString "UploadableAttachment {id = "
      . shows attch.id
      . showString ", filename = "
      . shows attch.filename
      . showString ", description = "
      . shows attch.description
      . showString ", content_type = "
      . shows attch.content_type
      . showString ", body = <RequestBody>}"

instance ToJSON UploadableAttachment where
  toJSON attch =
    A.object
      [ "id" A..= attch.id
      , "filename" A..= attch.filename
      , "description" A..= attch.description
      , "content_type" A..= attch.content_type
      ]

data CreateMessage = CreateMessage
  { content :: Maybe T.Text
  , nonce :: Maybe Int64
  , tts :: Maybe Bool
  , embeds :: Maybe (V.Vector Embed)
  , allowed_mentions :: Maybe (V.Vector AllowedMention)
  , message_reference :: Maybe MessageReference
  , components :: Maybe (V.Vector MessageComponent)
  , sticker_ids :: Maybe (V.Vector (ID Sticker))
  , attachments :: Maybe (V.Vector UploadableAttachment)
  , flags :: Maybe Int64
  , enforce_nonce :: Maybe Bool
  , poll :: Maybe Poll
  }
  deriving (Show, Generic)
  deriving (ToJSON) via DefaultJSON CreateMessage

newtype Sticker = Sticker Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

defaultCreateMessage :: CreateMessage
defaultCreateMessage =
  CreateMessage
    { content = Nothing
    , nonce = Nothing
    , tts = Nothing
    , embeds = Nothing
    , allowed_mentions = Nothing
    , message_reference = Nothing
    , components = Nothing
    , sticker_ids = Nothing
    , attachments = Nothing
    , flags = Nothing
    , enforce_nonce = Nothing
    , poll = Nothing
    }

newtype Poll = Poll Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

data Attachment = Attachment
  { id :: !(ID Attachment)
  , filename :: !FilePath
  , description :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype MessageComponent = MessageComponent Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype MessageReference = MessageReference Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Embed = Embed Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype AllowedMention = AllowedMention Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype DiscordBotToken = DiscordBotToken {rawToken :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

data User = User
  { id :: !(ID User)
  , username :: !T.Text
  , discriminator :: !T.Text
  , global_name :: !(Maybe T.Text)
  , avatar :: !(Maybe T.Text)
  , bot :: !(Maybe Bool)
  , system :: !(Maybe Bool)
  , mfa_enabled :: !(Maybe Bool)
  , banner :: !(Maybe T.Text)
  , accent_color :: !(Maybe Int64)
  , locale :: !(Maybe T.Text)
  , verified :: !(Maybe Bool)
  , email :: !(Maybe T.Text)
  , flags :: !(Maybe Int64)
  , premium_type :: !(Maybe Int64)
  , public_flags :: !(Maybe Int64)
  , avatar_decoration_data :: !(Maybe AvatarDecorationData)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AvatarDecorationData = AvatarDecorationData Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Timestamp = Timestamp {iso8601 :: ZonedTime}
  deriving (Generic)
  deriving newtype (Show, ISO8601)

instance FromJSON Timestamp where
  parseJSON = A.withText "ISO8601 Time" $ iso8601ParseM . T.unpack

instance ToJSON Timestamp where
  toJSON = A.toJSON . iso8601Show

newtype Role = Role Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Application = Application Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Webhook = Webhook Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

data Message = Message
  { id :: !(ID Message)
  , channel_id :: !(ID Channel)
  , content :: !T.Text
  , timestamp :: !Timestamp
  , author :: !User
  , edited_timestamp :: !(Maybe Timestamp)
  , tts :: !Bool
  , mention_everyone :: !Bool
  , mentions :: !(V.Vector User)
  , mention_roles :: !(V.Vector (ID Role))
  , mention_channels :: !(Maybe (V.Vector ChannelMention))
  , attachments :: !(V.Vector Attachment)
  , embeds :: !(V.Vector Embed)
  , reactions :: !(Maybe (V.Vector Reaction))
  , nonce :: !(Maybe Int64)
  , pinned :: !Bool
  , webhook_id :: !(Maybe Webhook)
  , type_ :: !Int
  , activity :: !(Maybe MessageActivity)
  , application :: !(Maybe Application)
  , application_id :: !(Maybe (ID Application))
  , message_reference :: !(Maybe MessageReference)
  , flags :: !(Maybe Int64)
  , referenced_message :: !(Maybe Message)
  , interaction_metadata :: !(Maybe Value)
  , interaction :: !(Maybe Value)
  , thread :: !(Maybe Channel)
  , components :: !(Maybe (V.Vector MessageComponent))
  , sticker_items :: !(Maybe (V.Vector Value))
  , stickers :: !(Maybe (V.Vector Value))
  , position :: !(Maybe Int64)
  , role_subscription_data :: !(Maybe Value)
  , poll :: !(Maybe Poll)
  , call :: !(Maybe Value)
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = A.genericParseJSON msgOpts

instance ToJSON Message where
  toJSON = A.genericToJSON msgOpts

msgOpts :: A.Options
msgOpts = A.defaultOptions {A.fieldLabelModifier = packed %~ T.dropWhileEnd (== '_')}

newtype Channel = Channel Value
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Reaction = Reaction Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype MessageActivity = MessageActivity Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype ChannelMention = ChannelMention Value
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

toUploadableAttachment :: Attachment -> UploadableAttachment
toUploadableAttachment attch =
  UploadableAttachment
    { id = attch.id
    , filename = attch.filename
    , description = attch.description
    , content_type = Nothing
    , body = Nothing
    }

createMessage ::
  (Http :> es, IOE :> es) =>
  DiscordConfig ->
  ID Channel ->
  CreateMessage ->
  Eff es Message
createMessage env channelID post = do
  let req0 = (mkDiscordRequest $ "/channels/" <> show channelID <> "/messages") {method = "POST"}
  req <- encodeAttachments post.attachments req0 post
  responseBody @Message <$> callDiscordAPIJSON env.discordToken req

editMessage ::
  (Http :> es, IOE :> es) =>
  DiscordConfig ->
  ID Channel ->
  ID Message ->
  EditMessage ->
  Eff es Message
editMessage env channelID msgID post = do
  let req0 = (mkDiscordRequest $ "/channels/" <> show channelID <> "/messages/" <> show msgID) {method = "PATCH"}
  req <- encodeAttachments post.attachments req0 post
  responseBody <$> callDiscordAPIJSON env.discordToken req

encodeAttachments ::
  (ToJSON a, IOE :> es) =>
  Maybe (V.Vector UploadableAttachment) ->
  Request ->
  a ->
  Eff es Request
encodeAttachments matts req payload
  | Just attchs <- matts
  , not $ V.null attchs = do
      let jsonPayload =
            (MP.partLBS "payload_json" $ A.encode payload)
              { MP.partContentType = Just "application/json"
              }
          files =
            V.toList $
              V.imapMaybe
                ( \i attch ->
                    attch.body <&> \body ->
                      ( MP.partFileRequestBody
                          (T.pack $ "files[" <> show i <> "]")
                          attch.filename
                          body
                      )
                        { MP.partContentType = TE.encodeUtf8 <$> attch.content_type
                        }
                )
                attchs
          ps = jsonPayload : files
      MP.formDataBody ps req {requestBody = ""}
  | otherwise =
      pure
        req
          { requestHeaders =
              ("Content-Type", "application/json")
                : req.requestHeaders
          , requestBody = RequestBodyLBS $ A.encode payload
          }

data EditMessage = EditMessage
  { content :: !(Maybe T.Text)
  , embeds :: !(Maybe (V.Vector Embed))
  , flags :: !(Maybe Int64)
  , allowed_mentions :: !(Maybe (V.Vector AllowedMention))
  , components :: !(Maybe (V.Vector MessageComponent))
  , attachments :: !(Maybe (V.Vector UploadableAttachment))
  }
  deriving (Show, Generic)
  deriving (ToJSON) via DefaultJSON EditMessage

deleteMessage :: (Http :> es) => DiscordConfig -> ID Channel -> ID Message -> Eff es ()
deleteMessage env channelID messageID = do
  let req =
        (mkDiscordRequest $ "/channels/" <> show channelID <> "/messages/" <> show messageID)
          { method = "DELETE"
          }
  void $ callDiscordAPI env.discordToken req

discordUrl :: String
discordUrl = "https://discord.com/api/v10"

mkDiscordRequest :: String -> Request
mkDiscordRequest ep = fromString $ discordUrl <> "/" <> dropWhile (== '/') ep

callDiscordAPIJSON ::
  (Http :> es, FromJSON a) =>
  DiscordBotToken ->
  Request ->
  Eff es (Response a)
callDiscordAPIJSON e =
  mapM (either throwString pure . A.eitherDecode)
    <=< (\r -> r <$ unsafeEff_ (print $ responseBody r))
    <=< callDiscordAPI e
    <=< (\rsp -> rsp <$ unsafeEff_ (print rsp))

callDiscordAPI ::
  (Http :> es) =>
  DiscordBotToken ->
  Request ->
  Eff es (Response LazyByteString)
callDiscordAPI tok req = do
  httpLbs
    req
      { requestHeaders =
          ("User-Agent", "DiscordUploader(https://github.com/konn/discord-uploader, 0.0.0.1)")
            : ("Authorization", "Bot " <> tok.rawToken)
            : req.requestHeaders
      }
