{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE CPP               #-}

module Meadowstalk.Static
  where

import Yesod.Static

-------------------------------------------------------------------------------

staticSite :: IO Static
staticSite =
#if defined(YESOD_DEVEL) || defined(GHCI)
  staticDevel "static"
#else
  static      "static"
#endif

$(staticFiles "static")
