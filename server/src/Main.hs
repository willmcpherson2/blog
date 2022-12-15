module Main where

import Data.Text (unpack)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, Request (pathInfo, requestMethod), Response, responseFile)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Prelude hiding (readFile)

main :: IO ()
main =
  getArgs >>= \case
    [html, js] -> do
      putStrLn $ "html: " <> html
      putStrLn $ "js: " <> js
      putStrLn "at http://localhost:8000/"
      run 8000 $ app html js
    _ -> putStrLn "no html/js to serve"

app :: String -> String -> Application
app html js request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  print (method, path)
  respond $ route html js path

route :: String -> String -> [String] -> Response
route html js = \case
  ["all.js"] -> responseFile status200 [(hContentType, "text/javascript")] js Nothing
  _ -> responseFile status200 [(hContentType, "text/html")] html Nothing
