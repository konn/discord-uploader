{-# LANGUAGE QuasiQuotes #-}

module Development.GitHub.Discord.Upload (
  defaultMain,
) where

import Control.Exception.Safe (throwString)
import Control.Lens
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Generics.Labels ()
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Environment
import Effectful.FileSystem.Tagged (Cwd, readFileBinaryLazy, runFileSystem)
import Effectful.Network.Cloudflare.Workers.KV
import Effectful.Network.Discord
import Effectful.Network.Http (RequestBody (..), runSimpleHttp)
import GHC.Generics (Generic)
import Path.Tagged (File, PathTo, RelTo, relfile)
import Text.Read (readEither)

newtype Notes = Notes {notes :: [Note]}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (A.FromJSON)

data Note = Note
  { name :: !T.Text
  , published :: !(Maybe Bool)
  , file :: PathTo Note (RelTo Cwd) File
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (A.FromJSON)

newtype Options = Options {config :: FilePath}
  deriving (Show, Eq, Ord, Generic)

defaultMain :: IO ()
defaultMain = runEff $ runEnvironment $ runSimpleHttp do
  env <- decodeDiscordConfig
  src <-
    runFileSystem $
      readFileBinaryLazy
        ([relfile|workspace/00-introduction-to-set-theory-and-logic.pdf|] :: PathTo "Image" (RelTo Cwd) File)
  msg <-
    createMessage
      env
      env.discordChannel
      ( defaultCreateMessage
          & #content ?~ "Howdy!?"
          & #attachments
            ?~ V.fromList
              [ UploadableAttachment
                  { id = 0
                  , filename = "00-introduction-to-set-theory-and-logic.pdf"
                  , description = Just "タマムシの図示"
                  , content_type = Just "image/png"
                  , body = Just $ RequestBodyLBS src
                  }
              ]
      )

  let edits =
        EditMessage
          { content = Just "Not Howdy!"
          , embeds = Nothing
          , flags = Nothing
          , allowed_mentions = Nothing
          , components = Nothing
          , attachments = Nothing
          }
  liftIO $ LBS.putStrLn $ A.encode edits

  runConcurrent $ threadDelay 5_000_000
  liftIO . print
    =<< editMessage
      env
      env.discordChannel
      msg.id
      edits

decodeDiscordConfig :: (Environment :> es) => Eff es DiscordConfig
decodeDiscordConfig = do
  discordAppID <- BS8.pack <$> getEnv "DISCORD_APP_ID"
  discordPubKey <- BS8.pack <$> getEnv "DISCORD_PUBLIC_KEY"
  discordToken <- DiscordBotToken . BS8.pack <$> getEnv "DISCORD_TOKEN"
  discordChannel <- either throwString pure . readEither =<< getEnv "DISCORD_CHANNEL"
  pure DiscordConfig {..}

data KVParams = KVParams
  { config :: !KVConfig
  , namespace :: !NamespaceID
  }
  deriving (Show, Eq, Ord, Generic)

decodeKVConfig :: (Environment :> es) => Eff es KVParams
decodeKVConfig = do
  accountId <- getEnv "CF_ACCOUNT_ID"
  namespace <- fromString <$> getEnv "CF_KV_NAMESPACE"
  apiKey <- fromString <$> getEnv "CF_API_KEY"
  pure KVParams {config = KVConfig {..}, ..}
