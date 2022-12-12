{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Reflex
import Reflex.Dom
import Text.Read (readMaybe)

main :: IO ()
main = mainWidget widget

widget :: (DomBuilder t m, PostBuild t m) => m ()
widget = el "div" $ do
  t <- inputElement def
  text ""
  dynText $ _inputElement_value t
