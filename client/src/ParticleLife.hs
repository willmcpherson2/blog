module ParticleLife (particleLife) where

import Debug.Trace (traceShowId, trace)
import Reflex.Dom (Widget, Element (..), elAttr', (=:), blank)
import JSDOM (currentWindowUnchecked)
import JSDOM.Types (liftJSM, FromJSVal (fromJSValUnchecked), ToJSVal (toJSVal), RenderingContext (..), CanvasRenderingContext2D, RequestAnimationFrameCallback (..), Callback (..), Window)
import JSDOM.Generated.HTMLCanvasElement (getContextUnchecked, HTMLCanvasElement, getWidth, getHeight, setWidth, setHeight)
import JSDOM.Generated.CanvasRenderingContext2D (beginPath, setFillStyle, fill, clearRect, stroke)
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
  deriving (Show)

data Point = Point {color :: Color, pos :: Vec, vel :: Vec}
  deriving (Show)

data Vec = Vec {x :: Double, y :: Double}
  deriving (Show)

data Color = Red | Yellow | Green | Cyan | Blue | Magenta
  deriving (Show)

particleLife :: Widget x ()
particleLife = do
  win <- currentWindowUnchecked
  canvas <- mkCanvas
  let state =
        State {points =
               [ Point {color = Red, pos = Vec 200 50, vel = Vec 0 0},
                 Point {color = Cyan, pos = Vec 0 50, vel = Vec 0 0},
                 Point {color = Green, pos = Vec 300 100, vel = Vec 0 0},
                 Point {color = Yellow, pos = Vec 200 200, vel = Vec 0 0},
                 Point {color = Yellow, pos = Vec 210 200, vel = Vec 0 0},
                 Point {color = Yellow, pos = Vec 205 210, vel = Vec 0 0}
               ]
              }
  let prevTime = Nothing
  liftJSM $ animate win canvas state prevTime

mkCanvas :: Widget x Canvas
mkCanvas = do
  (canvas, _) <- elAttr' "canvas" ("id" =: "canvas" <> "class" =: "particle-life") blank
  liftJSM $ do
    canvasVal <- toJSVal (_element_raw canvas)
    canvasEl <- fromJSValUnchecked canvasVal
    setWidth canvasEl 1000
    setHeight canvasEl 1000
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
  arc ctx (x $ pos point) (y $ pos point) radius 0 (2.0 * pi) False
  fill ctx (Just CanvasWindingRuleNonzero)
  stroke ctx

colorToString :: Color -> String
colorToString = \case
  Red -> "red"
  Yellow -> "yellow"
  Green -> "lime"
  Cyan -> "aqua"
  Blue -> "blue"
  Magenta -> "fuchsia"

update :: State -> Double -> State
update state delta = state{points = map (updatePoint delta (points state)) (points state)}

updatePoint :: Double -> [Point] -> Point -> Point
updatePoint delta others point =
  let vel' = foldr (updateVel delta point) (vel point) others
      pos' = Vec (x (pos point) + x vel') (y (pos point) + y vel')
   in point {vel = vel', pos = pos'}

updateVel :: Double -> Point -> Point -> Vec -> Vec
updateVel delta point other vel =
  let direction = normalize $ Vec (x (pos other) - x (pos point)) (y (pos other) - y (pos point))
      att = attraction (color other) (color point)
      dist = distance (pos point) (pos other)
      gravity = 1 / max 1 (dist ^ (2 :: Int))
      frictionRadius = radius * 20
      friction = (min frictionRadius (max 1 dist) - 1) / (frictionRadius - 1)
      speed = 2
      dx = x direction * att * gravity * friction * speed * delta
      dy = y direction * att * gravity * friction * speed * delta
   in Vec (x vel + dx) (y vel + dy)

distance :: Vec -> Vec -> Double
distance (Vec x1 y1) (Vec x2 y2) = sqrt $ (x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)

normalize :: Vec -> Vec
normalize (Vec x y) =
  let magnitude = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)
   in if magnitude == 0
         then Vec 0 0
         else Vec (x / magnitude) (y / magnitude)

attraction :: Color -> Color -> Double
attraction other color = matrix !! colorToIndex color !! colorToIndex other

radius :: Double
radius = 4

colorToIndex :: Color -> Int
colorToIndex = \case
  Red -> 0
  Yellow -> 1
  Green -> 2
  Cyan -> 3
  Blue -> 4
  Magenta -> 5

matrix :: [[Double]]
matrix =
  [ [ 0,  1,  0,  0,  0,  0], -- Red
    [ 0,  1,  0,  0,  0,  0], -- Yellow
    [ 0,  1,  0,  0,  0,  0], -- Green
    [ 0,  1,  0,  0,  0,  0], -- Cyan
    [ 0,  1,  0,  0,  0,  0], -- Blue
    [ 0,  1,  0,  0,  0,  0]  -- Magenta
  --  Red Yel Gre Cya Blu Mag
  ]
