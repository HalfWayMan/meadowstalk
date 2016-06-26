{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE CPP               #-}

module Meadowstalk.Application
  where

import Control.Monad.Logger

import Data.ByteString (ByteString)

import Database.Persist.Postgresql

import Yesod.Core

import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger

import Meadowstalk.Foundation
import Meadowstalk.Handlers.Public
import Meadowstalk.Model
import Meadowstalk.Static

-------------------------------------------------------------------------------

mkYesodDispatch "Meadowstalk" resourcesMeadowstalk

-------------------------------------------------------------------------------

makeApplication :: ByteString -> IO Application
makeApplication connstr = do
  static <- staticSite
  pool   <- runStdoutLoggingT $ do
    pool <- createPostgresqlPool connstr 20
    runSqlPool (runMigration migrateModel) pool
    return pool

  let foundation =
        Meadowstalk { siteStatic = static
                    , sitePool   = pool
                    }

  app <- toWaiAppPlain foundation
  return $ middleware $ app
  where
#if defined(YESOD_DEVEL) || defined(GHCI)
    logWare = logStdoutDev
#else
    logWare = logStdout
#endif

    middleware =
      acceptOverride . autohead . methodOverride . logWare
