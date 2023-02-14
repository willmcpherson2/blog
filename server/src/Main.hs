module Main (main) where

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
route dir [] = res dir "not-found.html" "text/html"
route dir (x : _) = case x of
  "posts" -> res dir "posts.html" "text/html"
  "posts.js" -> res dir "posts.js" "text/javascript"
  "music" -> res dir "music.html" "text/html"
  "music.js" -> res dir "music.js" "text/javascript"
  "markup-demo" -> res dir "markup-demo.html" "text/html"
  "markup-demo.js" -> res dir "markup-demo.js" "text/javascript"
  "tulip-demo" -> res dir "tulip-demo.html" "text/html"
  "tulip-demo.js" -> res dir "tulip-demo.js" "text/javascript"
  "particle-life" -> res dir "particle-life.html" "text/html"
  "particle-life.js" -> res dir "particle-life.js" "text/javascript"
  "style.css" -> res dir "style.css" "text/css"
  "favicon.ico" -> res dir "favicon.ico" "image/x-icon"
  "not-found.js" -> res dir "not-found.js" "text/javascript"
  _ -> res dir "not-found.html" "text/html"

res :: FilePath -> FilePath -> ByteString -> Response
res dir file mime =
  responseFile
    status200
    [(hContentType, mime)]
    (dir <> "/" <> file)
    Nothing
