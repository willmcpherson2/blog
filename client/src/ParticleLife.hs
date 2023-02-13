module ParticleLife (particleLife) where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShowId)
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (requestAnimationFrame_)
import JSDOM.Generated.CanvasPath (arc)
import JSDOM.Generated.CanvasRenderingContext2D
  ( beginPath,
    clearRect,
    fill,
    setFillStyle,
    stroke,
  )
import JSDOM.Generated.Enums (CanvasWindingRule (..))
import JSDOM.Generated.HTMLCanvasElement
  ( HTMLCanvasElement,
    getContextUnchecked,
    getHeight,
    getWidth,
    setHeight,
    setWidth,
  )
import JSDOM.Types
  ( Callback (..),
    CanvasRenderingContext2D,
    FromJSVal (fromJSValUnchecked),
    RenderingContext (..),
    RequestAnimationFrameCallback (..),
    ToJSVal (toJSVal),
    Window,
    liftJSM,
  )
import Language.Javascript.JSaddle.Object (function)
import Reflex.Dom (Element (..), Widget, blank, elAttr', (=:))

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
        State
          { points =
              [ Point {color = Red, pos = Vec 200 200, vel = Vec 0 0},
                Point {color = Green, pos = Vec 300 200, vel = Vec 0 0},
                Point {color = Blue, pos = Vec 250 250, vel = Vec 0 0}
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
  callback <- liftJSM $
    function $ \_ _ [nowTimeVal] -> do
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
update state delta = state {points = map (updatePoint delta (points state)) (points state)}

updatePoint :: Double -> [Point] -> Point -> Point
updatePoint delta others point =
  let vel' = normalize $ foldr (updateVel point) (Vec 0 0) others
      speed = 0.1
      pos' = Vec (x (pos point) + x vel' * speed * delta) (y (pos point) + y vel' * speed * delta)
   in point {vel = vel', pos = pos'}

updateVel :: Point -> Point -> Vec -> Vec
updateVel point other vel =
  let direction = Vec (x (pos other) - x (pos point)) (y (pos other) - y (pos point))
      att = attraction (color other) (color point)
   in Vec (x vel + x direction * att) (y vel + y direction * att)

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

{- ORMOLU_DISABLE -}
matrix :: [[Double]]
matrix =
  [ [ 0,  0,  1,  0, -1,  0], -- Red
    [ 0,  0,  0,  0,  0,  0], -- Yellow
    [-1,  0,  0,  0,  1,  0], -- Green
    [ 0,  0,  0,  0,  0,  0], -- Cyan
    [ 1,  0, -1,  0,  0,  0], -- Blue
    [ 0,  0,  0,  0,  0,  0]  -- Magenta
  --  Red Yel Gre Cya Blu Mag
  ]
{- ORMOLU_ENABLE -}
