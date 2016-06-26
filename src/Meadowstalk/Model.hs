{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meadowstalk.Model
  where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Persist.TH
import Database.Persist.Quasi

import Meadowstalk.Model.Types

-------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateModel"]
  $(persistFileWith lowerCaseSettings "config/model")

