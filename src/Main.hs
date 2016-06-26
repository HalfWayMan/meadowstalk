import Control.Monad.Logger
import Data.ByteString.Char8 (pack)
import Meadowstalk.Application
import Network.Wai.Handler.Warp
import System.Environment

-------------------------------------------------------------------------------

main :: IO ()
main = do
  port    <- read <$> getEnv "PORT"
  connstr <- pack <$> getEnv "DB"
  app     <- makeApplication connstr
  runSettings (setPort port defaultSettings) app
