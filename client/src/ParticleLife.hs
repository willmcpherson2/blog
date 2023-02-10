module ParticleLife (canvas) where

import Reflex.Dom
import JSDOM.Types (liftJSM, FromJSVal (fromJSValUnchecked), ToJSVal (toJSVal), RenderingContext (..), CanvasRenderingContext2D)
import JSDOM.Generated.HTMLCanvasElement (getContextUnchecked)
import JSDOM.Generated.CanvasRenderingContext2D (beginPath, setFillStyle, fill)
import JSDOM.Generated.CanvasPath (arc)
import JSDOM.Generated.Enums (CanvasWindingRule (..))

canvas :: Widget x ()
canvas = do
  ctx <- getCtx
  point ctx 50 50 20
  point ctx 100 100 20
  pure ()

getCtx :: Widget x CanvasRenderingContext2D
getCtx = do
  (canvas, _) <- elAttr' "canvas" ("id" =: "canvas") blank
  liftJSM $ do
    canvasVal <- toJSVal (_element_raw canvas)
    canvasEl <- fromJSValUnchecked canvasVal
    RenderingContext ctxVal <- getContextUnchecked canvasEl ("2d" :: String) ([] :: [String])
    fromJSValUnchecked ctxVal

point :: CanvasRenderingContext2D -> Double -> Double -> Double -> Widget x ()
point ctx x y radius = do
  setFillStyle ctx ("red" :: String)
  beginPath ctx
  arc ctx x y radius 0 (2.0 * pi) False
  fill ctx (Just CanvasWindingRuleNonzero)
