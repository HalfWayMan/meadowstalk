{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent

import System.Directory
import System.Environment
import System.Exit

import Network.Wai.Handler.Warp

import Meadowstalk.Application

main :: IO ()
main = do
  putStrLn "Starting Development Application"
  let connstr = "dbname=meadowstalk"
  port <- read <$> getEnv "PORT"
  app  <- makeApplication connstr
  forkIO (server port app)
  loop
  where
    server port =
      runSettings (setPort port defaultSettings)

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then exitSuccess else loop
