{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.Cloudflare.Worker.KVManager.Types (
  KVManager (..),
  ListKeys (..),
  ListKeyResult (..),
  ValueWithMetadata (..),
  PutOptions (..),
  Key (..),
  Cursor (..),
) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Cloudflare.Worker.Binding.KV
import Steward.Types

data KVManager mode = KVManager
  { delete :: mode ::: "delete" /> T.Text /> Delete NoContent
  , listKeys :: mode ::: "keys" /> QueryParam ListKeys /> Get (JSON ListKeyResult)
  , get :: mode ::: "values" /> T.Text /> Get (JSON (Maybe ValueWithMetadata))
  , put :: mode ::: "values" /> JSONBody PutOptions /> T.Text /> T.Text /> Put NoContent
  }
  deriving (Generic)
  deriving anyclass (HasHandler m, HasClient)
