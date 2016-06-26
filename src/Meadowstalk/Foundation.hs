{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Meadowstalk.Foundation
  where

import Data.Text (Text)
import Text.Hamlet

import Database.Persist.Sql

import Yesod.Core
import Yesod.Persist
import Yesod.Form
import Yesod.Static

import Meadowstalk.Model
import Meadowstalk.Static
import Meadowstalk.Template

-------------------------------------------------------------------------------

data Meadowstalk =
  Meadowstalk { siteStatic :: Static
              , sitePool   :: ConnectionPool
              }

-------------------------------------------------------------------------------

mkYesodData "Meadowstalk" $(parseRoutesFile "config/routes")
mkMessage "Meadowstalk" "messages" "en"

-------------------------------------------------------------------------------

instance Yesod Meadowstalk where
  defaultLayout widget = do
    current <- getCurrentRoute
    let isCurrent route = Just route == current
    PageContent title htags btags <- widgetToPageContent $ do
      $(lessFile "templates/bootstrap/bootstrap.less")
      $(lessFile "templates/font-awesome/font-awesome.less")
      $(lessFile "templates/layout/default-layout.less")
#ifndef YESOD_DEVEL
      addScript (StaticR ga_js)
#endif
      widget
    withUrlRenderer $(hamletFile "templates/layout/default-layout.hamlet")

-------------------------------------------------------------------------------

instance YesodPersist Meadowstalk where
  type YesodPersistBackend Meadowstalk = SqlBackend
  runDB action = do
    pool <- fmap sitePool getYesod
    runSqlPool action pool

-------------------------------------------------------------------------------

instance RenderMessage Meadowstalk FormMessage where
  renderMessage _ _ = defaultFormMessage
