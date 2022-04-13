{-# LANGUAGE OverloadedStrings #-}

import Compile (compile, compilePreview)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.List (sortOn)
import Data.Text (unpack)
import Network.HTTP.Types (Method, Status, hContentType, status200, status404)
import Network.Wai (Application, Request (pathInfo, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Parse (Document (title), parse)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)

type ContentType = BS.ByteString

type Content = ByteString

data Response = Response
  { status :: Status
  , contentType :: ContentType
  , content :: Content
  }

main :: IO ()
main = do
  port <- getPort
  putStrLn $ "running on port " <> show port
  run port app

getPort :: IO Port
getPort = maybe port read <$> lookupEnv portVar

app :: Application
app request respond = do
  let method = requestMethod request
      path = map unpack (pathInfo request)
  Response{status, contentType, content} <- router method path
  respond $ responseLBS status [(hContentType, contentType)] content

router :: Method -> [String] -> IO Response
router method path = case (method, path) of
  ("GET", []) -> serveIndex postsPath
  ("GET", ["posts"]) -> serveIndex postsPath
  ("GET", ["posts", title]) -> servePost title
  ("GET", ["www", "style.css"]) -> serveStyle
  _ -> serveNotFound

--------------------------------------------------------------------------------

serveIndex :: FilePath -> IO Response
serveIndex dirname = do
  filenames <- listDirectory dirname
  let paths = map (\filename -> dirname <> "/" <> filename) filenames
  previews <- reverse . sortOn fst <$> mapM getPreview paths
  let content = fromString $ concat $ map snd previews
  content' <- wrap content
  pure $ Response status200 textHtml content'
  where
    getPreview :: FilePath -> IO (Document, String)
    getPreview path = do
      content <- readFile path
      let document = parse content
          preview = compilePreview path (title document)
      pure (document, preview)

servePost :: FilePath -> IO Response
servePost filename = do
  fileExists <- doesFileExist $ postsPath <> "/" <> filename
  if fileExists
    then do
      content <- readFile $ postsPath <> "/" <> filename
      let content' = fromString $ compile $ parse $ content
      content'' <- wrap content'
      pure $ Response status200 textHtml content''
    else serveNotFound

serveStyle :: IO Response
serveStyle = do
  content <- LBS.readFile stylePath
  pure $ Response status200 textCss content

serveNotFound :: IO Response
serveNotFound = do
  content <- LBS.readFile notFoundPath
  content' <- wrap content
  pure $ Response status404 textHtml content'

--------------------------------------------------------------------------------

wrap :: Content -> IO Content
wrap content = do
  header <- LBS.readFile headerPath
  footer <- LBS.readFile footerPath
  pure $ header <> content <> footer

--------------------------------------------------------------------------------

port :: Port
port = 8080

portVar :: String
portVar = "PORT"

textHtml :: ContentType
textHtml = "text/html"

textCss :: ContentType
textCss = "text/css"

stylePath :: FilePath
stylePath = wwwPath <> "/style.css"

notFoundPath :: FilePath
notFoundPath = wwwPath <> "/not-found.html"

headerPath :: FilePath
headerPath = wwwPath <> "/header.html"

footerPath :: FilePath
footerPath = wwwPath <> "/footer.html"

postsPath :: FilePath
postsPath = "posts"

wwwPath :: FilePath
wwwPath = "www"
