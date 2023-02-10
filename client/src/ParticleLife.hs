module ParticleLife (particleLife) where

import Reflex.Dom (Widget, Element (..), elAttr', (=:), blank)
import JSDOM (currentWindowUnchecked)
import JSDOM.Types (liftJSM, FromJSVal (fromJSValUnchecked), ToJSVal (toJSVal), RenderingContext (..), CanvasRenderingContext2D, RequestAnimationFrameCallback (..), Callback (..), Window)
import JSDOM.Generated.HTMLCanvasElement (getContextUnchecked, HTMLCanvasElement, getWidth, getHeight)
import JSDOM.Generated.CanvasRenderingContext2D (beginPath, setFillStyle, fill, clearRect)
import JSDOM.Generated.CanvasPath (arc)
import JSDOM.Generated.Enums (CanvasWindingRule (..))
import JSDOM.Custom.Window (requestAnimationFrame_)
import Language.Javascript.JSaddle.Object (function)
import Data.Maybe (fromMaybe)

data Canvas = Canvas
  { canvasEl :: HTMLCanvasElement,
    ctx :: CanvasRenderingContext2D
  }

data State = State {points :: [Point]}

data Point = Point {color :: Color, pos :: Vec, vel :: Vec}

data Vec = Vec {x :: Double, y :: Double}

data Color = Red | Yellow | Green | Cyan | Blue | Magenta

particleLife :: Widget x ()
particleLife = do
  win <- currentWindowUnchecked
  canvas <- mkCanvas
  let state =
        State {points =
               [ Point {color = Red, pos = Vec 150 50, vel = Vec 0 0},
                 Point {color = Cyan, pos = Vec 100 50, vel = Vec 0 0}
               ]
              }
  let prevTime = Nothing
  liftJSM $ animate win canvas state prevTime

mkCanvas :: Widget x Canvas
mkCanvas = do
  (canvas, _) <- elAttr' "canvas" ("id" =: "canvas") blank
  liftJSM $ do
    canvasVal <- toJSVal (_element_raw canvas)
    canvasEl <- fromJSValUnchecked canvasVal
    RenderingContext ctxVal <- getContextUnchecked canvasEl ("2d" :: String) ([] :: [String])
    ctx <- fromJSValUnchecked ctxVal
    pure Canvas {canvasEl, ctx}

animate :: Window -> Canvas -> State -> Maybe Double -> IO ()
animate win canvas state prevTime = do
  callback <- liftJSM $ function $ \_ _ [nowTimeVal] -> do
    nowTime <- fromJSValUnchecked nowTimeVal
    animateCallback win canvas state prevTime nowTime
  requestAnimationFrame_ win (RequestAnimationFrameCallback $ Callback callback)

animateCallback :: Window -> Canvas -> State -> Maybe Double -> Double -> IO ()
animateCallback win canvas state prevTime nowTime = do
  let delta = nowTime - fromMaybe nowTime prevTime
  draw canvas state
  let state' = update state delta
  animate win canvas state' (Just nowTime)

draw :: Canvas -> State -> IO ()
draw canvas state = do
  width <- fromIntegral <$> getWidth (canvasEl canvas)
  height <- fromIntegral <$> getHeight (canvasEl canvas)
  clearRect (ctx canvas) 0 0 width height
  mapM_ (drawPoint canvas) (points state)

drawPoint :: Canvas -> Point -> IO ()
drawPoint Canvas {ctx} point = do
  setFillStyle ctx (colorToString (color point))
  beginPath ctx
  arc ctx (x $ pos point) (y $ pos point) 10 0 (2.0 * pi) False
  fill ctx (Just CanvasWindingRuleNonzero)

colorToString :: Color -> String
colorToString = \case
  Red -> "red"
  Yellow -> "yellow"
  Green -> "lime"
  Cyan -> "aqua"
  Blue -> "blue"
  Magenta -> "fuchsia"

update :: State -> Double -> State
update state delta = state{points = map (updatePoint state delta) (points state)}

updatePoint :: State -> Double -> Point -> Point
updatePoint state delta point = point{pos = Vec (x (pos point) + delta * 0.01) (y $ pos point)}
