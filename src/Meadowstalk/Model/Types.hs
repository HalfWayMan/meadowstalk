{-# LANGUAGE TemplateHaskell #-}

module Meadowstalk.Model.Types
  where

import Database.Persist.TH

-------------------------------------------------------------------------------

data AccessLevel
  = AdminAccess
  deriving (Eq, Read, Show)

derivePersistField "AccessLevel"
