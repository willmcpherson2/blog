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
    [dir] -> do
      putStrLn $ "dir: " <> dir
      putStrLn "at http://localhost:8000/"
      run 8000 $ app dir
    _ -> putStrLn "no html/js to serve"

app :: String -> Application
app dir request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  print (method, path)
  respond $ route dir path

route :: String -> [String] -> Response
route dir = \case
  ["all.js"] ->
    responseFile
      status200
      [(hContentType, "text/javascript")]
      (dir <> "/all.js")
      Nothing
  _ ->
    responseFile
      status200
      [(hContentType, "text/html")]
      (dir <> "/index.html")
      Nothing
