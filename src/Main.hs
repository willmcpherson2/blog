import Compile (compile, compileTitle)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text, unpack)
import Network.HTTP.Types (Method, Status, status200, status404)
import Network.Wai (Application, Request(pathInfo, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (run)
import Parse (parse)
import System.Directory (doesFileExist, listDirectory)

type Html = ByteString

type Path = [Text]

main :: IO ()
main = run 8080 app

app :: Application
app request respond = do
  let
    method = requestMethod request
    path = pathInfo request
  (status, response) <- router method path
  respond $ responseLBS status [("Content-Type", "text/html")] response

router :: Method -> Path -> IO (Status, Html)
router method path = case (method, path) of
  ("GET", []) -> do
    filePaths <- listDirectory "content/posts"
    titles <- mapM getTitle filePaths
    header <- readFile "www/header.html"
    footer <- readFile "www/footer.html"
    let html = header ++ concat titles ++ footer
    pure (status200, pack html)
  ("GET", ["www", "style.css"]) -> do
    css <- readFile "www/style.css"
    pure (status200, pack css)
  ("GET", ["posts", title]) -> do
    let filePath = "content/posts/" ++ unpack title
    exists <- doesFileExist filePath
    if exists
      then do
        text <- readFile filePath
        header <- readFile "www/header.html"
        footer <- readFile "www/footer.html"
        let
          document = parse text
          html = header ++ compile document ++ footer
        pure (status200, pack html)
      else notFound
  _ -> notFound

getTitle :: FilePath -> IO String
getTitle filePath = do
  text <- readFile $ "content/posts/" ++ filePath
  let
    document = parse text
    title = compileTitle document
  pure title

notFound :: IO (Status, ByteString)
notFound = do
  text <- readFile "content/404"
  header <- readFile "www/header.html"
  footer <- readFile "www/footer.html"
  let
    document = parse text
    html = header ++ compile document ++ footer
  pure (status404, pack html)
