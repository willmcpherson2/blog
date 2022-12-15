module Post (Post (..)) where

import Data.Function (on)
import Data.Time (Day)

data Post = Post
  { key :: String,
    date :: Day,
    title :: String,
    content :: String
  }
  deriving (Eq)

instance Ord Post where
  compare = compare `on` date
