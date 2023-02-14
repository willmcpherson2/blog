module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Data.ByteString (ByteString)
import Data.Text (unpack)
import Network.HTTP.Types
  ( hContentType,
    hLocation,
    status200,
    status301,
    status404,
  )
import Network.Wai
  ( Application,
    Request (pathInfo, requestMethod),
    Response,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.FilePath (splitExtension)

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
  route dir path >>= respond

route :: String -> [String] -> IO Response
route dir = \case
  [] ->
    pure $
      responseLBS
        status301
        [(hContentType, "text/plain"), (hLocation, "/posts")]
        "Redirect"
  segment : _ ->
    let (basename, extension) = splitExtension segment
     in case extension of
          "" -> routeFile dir (basename <> ".html") "text/html"
          ".js" -> routeFile dir (basename <> ".js") "text/javascript"
          ".css" -> routeFile dir (basename <> ".css") "text/css"
          ".ico" -> routeFile dir (basename <> ".ico") "image/x-icon"
          _ -> pure $ notFound dir

routeFile :: FilePath -> FilePath -> ByteString -> IO Response
routeFile dir file mime = do
  let path = dir <> "/" <> file
  doesFileExist path >>= \case
    True ->
      pure $
        responseFile
          status200
          [(hContentType, mime)]
          path
          Nothing
    False -> pure $ notFound dir

notFound :: FilePath -> Response
notFound dir =
  responseFile
    status404
    [(hContentType, "text/html")]
    (dir <> "/" <> "not-found.html")
    Nothing
