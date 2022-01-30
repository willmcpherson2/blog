import Compile (compile)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text, unpack)
import Network.HTTP.Types (Method, Status, status200, status404)
import Network.Wai (Application, Request(pathInfo, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (run)
import Parse (parse)
import System.Directory (doesFileExist)

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
  ("GET", []) -> pure (status200, index)
  ("GET", ["www", "style.css"]) -> do
    css <- readFile "www/style.css"
    pure (status200, pack css)
  ("GET", ["posts", title]) -> do
    let path = "content/posts/" ++ unpack title
    exists <- doesFileExist path
    if exists
      then do
        text <- readFile path
        header <- readFile "www/header.html"
        footer <- readFile "www/footer.html"
        let
          document = parse text
          html = header ++ compile document ++ footer
        pure (status200, pack html)
      else pure (status404, response404)
  _ -> pure (status404, response404)

response404 :: Html
response404 = "<h1>404</h1>"

index :: Html
index = "<h1>index</h1>"
