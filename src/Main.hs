{-# LANGUAGE OverloadedStrings #-}

import Compile (compile, compilePreview)
import Control.Monad ((<=<))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text, unpack)
import Network.HTTP.Types (Method, Status, status200, status404)
import Network.Wai (Application, Request (pathInfo, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Parse (parse)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)

type Content = ByteString

type Path = [Text]

main :: IO ()
main = do
  port <- getPort
  run port app

getPort :: IO Port
getPort = maybe 8080 read <$> lookupEnv "PORT"

app :: Application
app request respond = do
  let method = requestMethod request
      path = pathInfo request
  (status, response) <- router method path
  respond $ responseLBS status [("Content-Type", "text/html")] response

router :: Method -> Path -> IO (Status, Content)
router method path = case (method, path) of
  ("GET", []) -> index "posts"
  ("GET", ["posts"]) -> index "posts"
  ("GET", ["posts", title]) -> serveFilePath $ "posts/" ++ unpack title
  ("GET", ["www", "style.css"]) -> serveRaw "www/style.css"
  _ -> notFound

--------------------------------------------------------------------------------

index :: FilePath -> IO (Status, Content)
index directory = do
  filePaths <- directoryPaths directory
  titles <- mapM getTitle filePaths
  let document = concat titles
  html <- wrap document
  pure (status200, html)
  where
    getTitle :: FilePath -> IO String
    getTitle filePath = do
      text <- readFile filePath
      let document = parse text
          title = compilePreview document filePath
      pure title

    directoryPaths :: FilePath -> IO [FilePath]
    directoryPaths filePath = do
      filePaths <- listDirectory filePath
      let filePaths' = map (\filename -> filePath ++ "/" ++ filename) filePaths
      pure filePaths'

serveRaw :: FilePath -> IO (Status, Content)
serveRaw filePath = do
  content <- readFile filePath
  pure (status200, pack content)

serveFilePath :: FilePath -> IO (Status, Content)
serveFilePath filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      html <- filePathToHtml filePath
      pure (status200, html)
    else notFound

notFound :: IO (Status, Content)
notFound = do
  html <- filePathToHtml "www/404"
  pure (status404, html)

--------------------------------------------------------------------------------

filePathToHtml :: FilePath -> IO Content
filePathToHtml = textToHtml <=< readFile

textToHtml :: String -> IO Content
textToHtml = wrap . compile . parse

wrap :: String -> IO Content
wrap document = do
  htmlHeader <- readFile "www/header.html"
  header <- compile . parse <$> readFile "www/header"
  footer <- compile . parse <$> readFile "www/footer"
  htmlFooter <- readFile "www/footer.html"
  let wrapped = htmlHeader ++ header ++ document ++ footer ++ htmlFooter
      html = pack wrapped
  pure html
