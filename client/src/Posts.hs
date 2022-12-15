module Posts (posts) where

import Data.Time (fromGregorian)
import Post (Post (..))

posts :: [Post]
posts =
  [ Post "post-1" (fromGregorian 2022 2 1) "Post 1" "Hello, this is a post",
    Post "post-2" (fromGregorian 2022 12 15) "Post 2" "Hello, this is another post"
  ]
