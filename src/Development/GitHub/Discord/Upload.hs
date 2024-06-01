{-# LANGUAGE QuasiQuotes #-}

module Development.GitHub.Discord.Upload (
  defaultMain,
  defaultMainWith,
  Options (..),
  optionsP,
) where

import Control.Applicative ((<**>))
import Control.Exception.Safe (Exception (..), throwString, tryAny)
import Control.Lens
import Control.Monad (forM_, guard)
import Data.Aeson qualified as A
import Data.Bifunctor qualified as Bi
import Data.ByteString.Char8 qualified as BS8
import Data.Digest.Pure.SHA.Streaming (sha256Stream_, showDigest)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashSet qualified as HS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Time.Format
import Data.Vector qualified as V
import Data.Yaml.Aeson qualified as Y
import Effectful
import Effectful.Environment
import Effectful.FileSystem.Tagged (Cwd, makeAbsolute, readFileBinaryStrict, runFileSystem)
import Effectful.Log.Extra (Log, LogLevel (..), localDomain, logInfo_, logTrace, runStdErrLogger)
import Effectful.Network.Cloudflare.Workers.KV
import Effectful.Network.Discord
import Effectful.Network.Http (Http, runSimpleHttp, toRequestBody)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Resource
import Effectful.Time (Clock, getZonedTime, runClock)
import GHC.Generics (Generic)
import Options.Applicative qualified as Opts
import Path.Tagged (File, PathTo, RelTo, SomeBase (..), fromRelFile, parseSomeFile, relfile)
import Streaming.ByteString qualified as EffQ
import Text.Read (readEither)

newtype Notes = Notes {notes :: [Note]}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (A.FromJSON)

data Note = Note
  { name :: !T.Text
  , file :: PathTo Note (RelTo Cwd) File
  , draft :: !(Maybe Bool)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (A.FromJSON)

newtype Options = Options {config :: SomeBase Cwd Note File}
  deriving (Show, Eq, Ord, Generic)

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opts.execParser optionsP

optionsP :: Opts.ParserInfo Options
optionsP =
  Opts.info (p <**> Opts.helper) $
    mconcat
      [ Opts.fullDesc
      , Opts.progDesc "Upload notes to Discord"
      , Opts.header "discord-upload - upload notes to Discord"
      ]
  where
    p =
      Options
        <$> Opts.option
          ( Opts.eitherReader $
              Bi.first displayException . parseSomeFile
          )
          ( Opts.long "config"
              <> Opts.short 'c'
              <> Opts.metavar "PATH"
              <> Opts.value (IsRel [relfile|notes.yaml|])
              <> Opts.showDefault
              <> Opts.help "Path to the configuration file"
          )

defaultMainWith :: Options -> IO ()
defaultMainWith opts = runEff $ runEnvironment $ runSimpleHttp do
  discord <- decodeDiscordConfig
  kvs <- decodeKVConfig
  notes <-
    runFileSystem $
      Y.decodeThrow @_ @Notes
        =<< readFileBinaryStrict
        =<< makeAbsolute opts.config

  runClock $
    runStdErrLogger "uploader" LogTrace $
      runSimpleHttp $
        runReader discord $
          runReader kvs $
            runKV kvs.config $ do
              runResource $
                runFileSystem $
                  mapM_ uploadNote notes.notes
              pruneUnusedKeys notes

pruneUnusedKeys :: (KV :> es, Reader KVParams :> es, Log :> es, Http :> es, Reader DiscordConfig :> es) => Notes -> Eff es ()
pruneUnusedKeys notes = localDomain "pruneUnusedKeys" do
  let living = HS.fromList $ mapMaybe (\note -> noteKey note <$ guard (Just True /= note.draft)) notes.notes
  ns <- EffL.view @KVParams #namespace
  oldKeys <-
    maybe mempty (HS.fromList . map (.name)) . (.result)
      <$> listKeys ns ListKeysOptions {prefix = Nothing, limit = Nothing, cursor = Nothing}
  let redundants = oldKeys `HS.difference` living
  logTrace "living keys" living
  logTrace "old keys" oldKeys
  logInfo_ $ "Cleaning Redundant keys: " <> T.pack (show redundants)
  disco <- ask @DiscordConfig
  forM_ redundants $ \key -> do
    logTrace "Deleting key" key
    m <- getKeyValue @Message ns key
    forM_ m \resl ->
      deleteMessage disco resl.metadata.channel_id resl.metadata.id
    void $ tryAny $ deleteKey ns key

noteAnnounce ::
  (Http :> es, IOE :> es, Resource :> es, Clock :> es, Log :> es) =>
  Note ->
  Maybe (KeyEntry Message) ->
  Eff es (Maybe (CreateMessage, T.Text))
noteAnnounce note oldEntry = localDomain "noteAnnounce" do
  digest <-
    EffQ.readFile (fromRelFile note.file)
      & sha256Stream_
      <&> T.pack . showDigest
  if Just digest == ((.value) <$> oldEntry)
    then Nothing <$ logInfo_ "No changes"
    else
      Just <$> do
        body <- fmap Just $ toRequestBody $ EffQ.readFile (fromRelFile note.file)
        now <- getZonedTime
        let content =
              T.unlines
                [ "# " <> note.name
                , ""
                , "📁 File: `" <> fromString (fromRelFile note.file) <> "`"
                , "📆 Last Update: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now)
                ]
            attach =
              UploadableAttachment
                { content_type = Just "application/pdf"
                , id = 0
                , filename = fromRelFile note.file
                , description = Just note.name
                , ..
                }
        let msg =
              defaultCreateMessage
                & #attachments ?~ V.singleton attach
                & #content ?~ content

        pure (msg, digest)

uploadNote ::
  ( Reader KVParams :> es
  , KV :> es
  , Resource :> es
  , Reader DiscordConfig :> es
  , Http :> es
  , IOE :> es
  , Clock :> es
  , Log :> es
  ) =>
  Note ->
  Eff es ()
uploadNote note
  | fromMaybe False note.draft = pure ()
  | otherwise = localDomain ("note: " <> note.name) do
      ns <- EffL.view @KVParams #namespace
      disco <- ask @DiscordConfig
      let key = noteKey note
      mvmeta <- getKeyValue @Message ns key
      mProc <- noteAnnounce note mvmeta
      forM_ mProc \(msgBody, digest) -> do
        case mvmeta of
          Nothing -> do
            logInfo_ "No key found"
            msg <- createMessage disco disco.discordChannel msgBody
            void $ putKeyValue ns (noteKey note) (Just msg) digest
            pinMessage disco disco.discordChannel msg.id
          Just msg -> do
            logInfo_ "Key found"
            newMsg <-
              editMessage disco disco.discordChannel msg.metadata.id $
                defaultEditMessage
                  & #content .~ msgBody.content
                  & #attachments .~ msgBody.attachments
            void $ putKeyValue ns (noteKey note) (Just newMsg) digest

noteKey :: Note -> Key
noteKey = fromString . fromRelFile . (.file)

decodeDiscordConfig :: (Environment :> es) => Eff es DiscordConfig
decodeDiscordConfig = do
  discordAppID <- BS8.pack <$> getEnv "DISCORD_APP_ID"
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
