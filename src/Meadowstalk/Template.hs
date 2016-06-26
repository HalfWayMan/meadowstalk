{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Meadowstalk.Template
  ( lessFile
  ) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Process

import           Yesod.Core

-------------------------------------------------------------------------------

#if defined(YESOD_DEVEL) || defined(GHCI)

lessFile :: FilePath -> Q Exp
lessFile path =
  lessFileArgs path []

lessFileArgs :: FilePath -> [String] -> Q Exp
lessFileArgs path args = do
  qAddDependentFile path
  (`AppE` (ListE $ map (LitE . StringL) (args ++ defaultLessArgs))) . (`AppE` (LitE $ StringL path)) <$> [| lessRuntime |]

lessRuntime :: (Yesod master) => FilePath -> [String] -> WidgetT master IO ()
lessRuntime path args = do
  content <- liftIO $ (readProcess lessCommand (args ++ ["-"]) =<< readFile path)
  toWidgetHead (CssBuilder (LTB.fromLazyText (LT.pack content)))

#else

lessFile :: FilePath -> Q Exp
lessFile file =
  lessFileArgs file ["-x"]

lessFileArgs :: FilePath -> [String] -> Q Exp
lessFileArgs file args = do
  qAddDependentFile file
  content <- qRunIO (readProcess lessCommand (args ++ defaultLessArgs ++ ["-"]) =<< readFile file)
  (`AppE` (LitE $ StringL content)) <$> [| lessRuntime |]

lessRuntime :: (Yesod master) => String -> WidgetT master IO ()
lessRuntime content = do
  toWidgetHead (CssBuilder (LTB.fromLazyText (LT.pack content)))

#endif

defaultLessArgs :: [String]
defaultLessArgs =
  ["--include-path=templates/"]

lessCommand :: String
#ifdef WINDOWS
lessCommand = "C:/msys64/mingw64/bin/lessc.cmd"
#else
lessCommand = "lessc"
#endif
