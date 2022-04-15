{-# LANGUAGE OverloadedStrings #-}

import Compile (compile, compilePreview, escape)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List (sortOn)
import Data.String (IsString)
import Data.Text (unpack)
import Network.HTTP.Types (Method, Status, hContentType, status200, status404)
import Network.Wai (Application, Request (pathInfo, requestMethod), lazyRequestBody, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Parse (Document (title), parse)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import Tulip

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
  body <- lazyRequestBody request
  Response{status, contentType, content} <- router method path body
  respond $ responseLBS status [(hContentType, contentType)] content

router :: Method -> [String] -> ByteString -> IO Response
router method path body = case (method, path) of
  ("GET", []) -> getIndex postsPath
  ("GET", ["posts"]) -> getIndex postsPath
  ("GET", ["posts", title]) -> getPost title
  ("GET", ["tulip"]) -> getTulip
  ("POST", ["tulip"]) -> postTulip body
  ("GET", ["www", "style.css"]) -> getStyle
  _ -> getNotFound

--------------------------------------------------------------------------------

getIndex :: FilePath -> IO Response
getIndex dirname = do
  filenames <- listDirectory dirname
  let paths = map (\filename -> dirname `slash` filename) filenames
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

getPost :: FilePath -> IO Response
getPost filename = do
  fileExists <- doesFileExist $ postsPath `slash` filename
  if fileExists
    then do
      content <- readFile $ postsPath `slash` filename
      let content' = fromString $ compile $ parse $ content
      content'' <- wrap content'
      pure $ Response status200 textHtml content''
    else getNotFound

getTulip :: IO Response
getTulip = do
  content <- LBS.readFile tulipPath
  content' <- wrap content
  pure $ Response status200 textHtml content'

postTulip :: ByteString -> IO Response
postTulip body = do
  let result = escape $ getResult $ toString body
  pure $ Response status200 textPlain (fromString result)

getStyle :: IO Response
getStyle = do
  content <- LBS.readFile stylePath
  pure $ Response status200 textCss content

getNotFound :: IO Response
getNotFound = do
  content <- LBS.readFile notFoundPath
  content' <- wrap content
  pure $ Response status404 textHtml content'

--------------------------------------------------------------------------------

wrap :: Content -> IO Content
wrap content = do
  header <- LBS.readFile headerPath
  footer <- LBS.readFile footerPath
  pure $ header <> content <> footer

slash :: (Semigroup a, IsString a) => a -> a -> a
slash x y = x <> "/" <> y

--------------------------------------------------------------------------------

port :: Port
port = 8080

portVar :: String
portVar = "PORT"

textHtml :: ContentType
textHtml = "text/html"

textPlain :: ContentType
textPlain = "text/plain"

textCss :: ContentType
textCss = "text/css"

stylePath :: FilePath
stylePath = wwwPath `slash` "style.css"

notFoundPath :: FilePath
notFoundPath = wwwPath `slash` "not-found.html"

headerPath :: FilePath
headerPath = wwwPath `slash` "header.html"

footerPath :: FilePath
footerPath = wwwPath `slash` "footer.html"

tulipPath :: FilePath
tulipPath = wwwPath `slash` "tulip.html"

postsPath :: FilePath
postsPath = "posts"

wwwPath :: FilePath
wwwPath = "www"
