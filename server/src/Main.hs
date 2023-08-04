module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Text (unpack)
import Network.HTTP.Types
  ( Status,
    hContentType,
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
      putStrLn dir
      putStrLn $ "http://localhost:" <> show port
      run port (app dir)
    _ -> putStrLn "no directory provided"

app :: String -> Application
app dir request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  print (method, path)
  route dir path >>= respond

route :: String -> [String] -> IO Response
route dir = \case
  [] -> routeFile dir "/index.html" status200
  path -> routeFile dir (intercalate "/" path) status200

routeFile :: FilePath -> FilePath -> Status -> IO Response
routeFile dir file status = do
  let (_, extension) = splitExtension file
      mime =
        case extension of
          "" -> "text/html"
          ".html" -> "text/html"
          ".js" -> "text/javascript"
          ".css" -> "text/css"
          ".ico" -> "image/x-icon"
          _ -> "text/plain"
      path =
        case extension of
          "" -> dir <> "/" <> file <> ".html"
          _ -> dir <> "/" <> file
  doesFileExist path >>= \case
    True ->
      pure $
        responseFile
          status
          [(hContentType, mime)]
          path
          Nothing
    False -> routeFile dir "not-found" status404
