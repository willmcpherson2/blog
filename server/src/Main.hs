module Main where

import Control.Applicative (Alternative ((<|>)))
import Data.ByteString (ByteString)
import Data.Text (unpack)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, Request (pathInfo, requestMethod), Response, responseFile)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs, getEnv)
import Prelude hiding (readFile)

main :: IO ()
main =
  getArgs >>= \case
    [dir] -> do
      port <- read <$> getEnv "PORT" <|> pure 8000
      putStrLn $ "dir: " <> dir
      putStrLn $ "at http://localhost:" <> show port
      run port (app dir)
    _ -> putStrLn "no html/js to serve"

app :: String -> Application
app dir request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  print (method, path)
  respond $ route dir path

route :: String -> [String] -> Response
route dir = \case
  ["style.css"] -> res dir "style.css" "text/css"
  ["all.js"] -> res dir "all.js" "text/javascript"
  ["favicon.ico"] -> res dir "favicon.ico" "image/x-icon"
  _ -> res dir "/index.html" "text/html"

res :: FilePath -> FilePath -> ByteString -> Response
res dir file mime =
  responseFile
    status200
    [(hContentType, mime)]
    (dir <> "/" <> file)
    Nothing
