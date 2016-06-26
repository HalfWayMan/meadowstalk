{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meadowstalk.Views
  where

import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Persist.TH
import Database.Persist.Quasi

import Meadowstalk.Model

-------------------------------------------------------------------------------

share [mkPersist sqlSettings]
  $(persistFileWith lowerCaseSettings "config/views")

