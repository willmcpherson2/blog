module Post (Post (..), posts) where

import Data.Time (Day, fromGregorian)
import Reflex.Dom (Widget)
import qualified Post.HelloWorld

data Post = Post
  { key :: String,
    date :: Day,
    title :: String,
    content :: forall x. Widget x ()
  }

posts :: [Post]
posts =
  [ Post "hello-world" (fromGregorian 2022 2 1) "Hello World" Post.HelloWorld.post
  ]
