{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Monad
import Control.Monad.Logger
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
import Meadowstalk.Model
import System.Environment

-------------------------------------------------------------------------------

displayUsage :: IO ()
displayUsage =
  putStrLn "usage: migrator <connection-string>"

withConnStr :: (String -> IO a) -> IO ()
withConnStr action = do
  args <- getArgs
  case args of
    [ connstr ] -> void (action connstr)
    _ -> displayUsage

main :: IO ()
main =
  withConnStr $ \connstr ->
    runStdoutLoggingT $ withPostgresqlPool (pack connstr) 1 $
      runSqlPool (runMigration migrateModel)
