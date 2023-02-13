module Main (main) where

import Reflex.Dom
import Markup.M (m)
import Utils (header)

main :: IO ()
main = mainWidget $ do
  header
  [m|

My music is licensed under @[[Creative Commons Attribution][https://creativecommons.org/licenses/by/3.0/]]

p[
@[[Google Drive (absolutely everything)][https://drive.google.com/drive/folders/1uEvXx650Y0JK6Y-4HOhXSf9ibF5eKhh0?usp=sharing]]
]

p[
@[[cereal7.bandcamp.com][https://cereal7.bandcamp.com]]
]

p[
@[[soundcloud.com/cereal7][https://soundcloud.com/cereal7]]
]

|]
