module Main (main) where

import Data.List (find, sortOn)
import Data.Text (pack)
import Data.Time (Day, fromGregorian)
import qualified HelloWorld
import Reflex.Dom
import Utils (route, header)

main :: IO ()
main = mainWidget $ do
  header
  route $ \case
    ["posts"] -> previews
    ["posts", name] -> post name

data Post = Post
  { key :: String,
    date :: Day,
    title :: String,
    content :: forall x. Widget x ()
  }

previews :: Widget x ()
previews = mapM_ preview $ sortOn date posts

preview :: Post -> Widget x ()
preview post = elClass "div" "preview" $ do
  elAttr "a" ("href" =: pack ("/posts/" <> key post)) $ text $ pack $ title post
  text " "
  el "date" $ text $ pack $ show $ date post

post :: String -> Widget x ()
post k = case find (\p -> key p == k) posts of
  Just p -> do
    el "h1" $ text $ pack $ title p
    el "date" $ text $ pack $ show $ date p
    content p
  Nothing -> undefined

posts :: [Post]
posts =
  [ Post "hello-world" (fromGregorian 2022 12 27) "Hello World" HelloWorld.post
  ]
