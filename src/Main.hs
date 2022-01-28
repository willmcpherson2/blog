import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Types (Method, Status, status200, status404)
import Network.Wai (Application, Request(pathInfo, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (run)

type Path = [Text]

type Response = ByteString

main :: IO ()
main = run 8080 app

app :: Application
app request respond = do
  let
    method = requestMethod request
    path = pathInfo request
    (status, response) = router method path
  respond $ responseLBS status [("Content-Type", "text/html")] response

router :: Method -> Path -> (Status, Response)
router method path = case (method, path) of
  ("GET", []) -> (status200, index)
  ("GET", ["posts"]) -> (status200, posts)
  _ -> (status404, response404)

posts :: Response
posts = "<h1>posts...</h1>"

response404 :: Response
response404 = "<h1>404</h1>"

index :: Response
index = "<h1>Hello, Web!</h1>"
