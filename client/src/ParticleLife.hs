module ParticleLife (canvas) where

import Debug.Trace (traceShowId)
import Reflex.Dom (Widget, Element (..), elAttr', (=:), blank)
import JSDOM (currentWindowUnchecked)
import JSDOM.Types (liftJSM, FromJSVal (fromJSValUnchecked), ToJSVal (toJSVal), RenderingContext (..), CanvasRenderingContext2D, RequestAnimationFrameCallback (..), Callback (..), Window)
import JSDOM.Generated.HTMLCanvasElement (getContextUnchecked)
import JSDOM.Generated.CanvasRenderingContext2D (beginPath, setFillStyle, fill)
import JSDOM.Generated.CanvasPath (arc)
import JSDOM.Generated.Enums (CanvasWindingRule (..))
import JSDOM.Custom.Window (requestAnimationFrame_)
import Language.Javascript.JSaddle.Object (function)
import Data.Maybe (fromMaybe)

data State = State {x :: Double}

canvas :: Widget x ()
canvas = do
  win <- currentWindowUnchecked
  ctx <- getCtx
  let state = State {x = 0}
  let prevTime = Nothing
  liftJSM $ animate win ctx state prevTime
  pure ()

getCtx :: Widget x CanvasRenderingContext2D
getCtx = do
  (canvas, _) <- elAttr' "canvas" ("id" =: "canvas") blank
  liftJSM $ do
    canvasVal <- toJSVal (_element_raw canvas)
    canvasEl <- fromJSValUnchecked canvasVal
    RenderingContext ctxVal <- getContextUnchecked canvasEl ("2d" :: String) ([] :: [String])
    fromJSValUnchecked ctxVal

point :: CanvasRenderingContext2D -> Double -> Double -> IO ()
point ctx x y = do
  setFillStyle ctx ("red" :: String)
  beginPath ctx
  arc ctx x y 10 0 (2.0 * pi) False
  fill ctx (Just CanvasWindingRuleNonzero)

animate :: Window -> CanvasRenderingContext2D -> State -> Maybe Double -> IO ()
animate win ctx state prevTime = do
  callback <- liftJSM $ function $ \_ _ [nowTimeVal] -> do
    nowTime <- fromJSValUnchecked nowTimeVal
    draw win ctx state prevTime nowTime
  requestAnimationFrame_ win (RequestAnimationFrameCallback $ Callback callback)

draw :: Window -> CanvasRenderingContext2D -> State -> Maybe Double -> Double -> IO ()
draw win ctx state prevTime nowTime = do
  point ctx (x state) 50
  let delta = nowTime - fromMaybe nowTime prevTime
  let state' = state{x = x state + delta * 0.01}
  animate win ctx state' (Just nowTime)
  pure ()
