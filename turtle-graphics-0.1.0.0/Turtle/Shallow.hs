-- | A simple library for turtle graphics. Shallow embedding.
module Turtle.Shallow
       (
         -- * Turtle graphics program type
         Program
         , Turtle(..)

         -- * Primitive operations
         , newTurtle
         , forward
         , backward
         , right
         , left

         , penup
         , pendown

         , color
         , colorBy

         , times
         , forever

         , (<|>)
         ) where

import Text.Printf
import qualified Graphics.Rendering.OpenGL as OpenGL
import Control.Monad (forM, mapM)

-- | Turtle data type
type Position = (Double, Double)
type Orientation = Double
type Color = (Double, Double, Double)
data Turtle = Turtle { pos :: Position
                     , dir :: Orientation
                     , pen :: Bool
                     , col :: Color
                     }
setPos :: Turtle -> Position -> Turtle
setPos (Turtle _ dir pen col) pos = Turtle pos dir pen col

setDir :: Turtle -> Orientation -> Turtle
setDir (Turtle pos _ pen col) dir = Turtle pos dir pen col

setPen :: Turtle -> Bool -> Turtle
setPen (Turtle pos dir _ col) pen = Turtle pos dir pen col

setCol :: Turtle -> Color -> Turtle
setCol (Turtle pos dir pen _) col = Turtle pos dir pen col

instance Show Turtle where
  show (Turtle (x,y) dir _ _) = printf "Turtle {(%.2f, %.2f) %.2f}" x y (degrees dir)

type Program = [Turtle] -> IO [Turtle]

-- Primitive operations
newTurtle :: Turtle
newTurtle = Turtle { pos = (0.0, 0.0)
                   , dir = radians 90.0
                   , pen = True
                   , col = (1.0, 1.0, 1.0)
                   }

forward :: Double -> Program
forward dist ts = mapM (forward_ dist) ts where
  forward_ dist orig@(Turtle (x1, y1) theta pen col@(r, g, b)) = do
    let next = setPos orig (x1 + dist * cos(theta), y1 + dist * sin(theta))
    putStrLn $ printf "forward %.2f: %s -> %s" dist (show orig) (show next)
    if (pen == True) then
      OpenGL.renderPrimitive OpenGL.Lines $ do
        OpenGL.color $ (OpenGL.Color3 r g b)
        mapM_ toVertex [orig, next]
      else return ()
    return next

  toVertex t = let (x, y) = pos t
               in OpenGL.vertex $ OpenGL.Vertex3 x y 0

backward :: Double -> Program
backward = forward . ((-1.0) *)

right :: Double -> Program
right angle ts = mapM (right_ angle) ts where
  right_ angle orig@(Turtle pos theta pen col) = do
    let next = setDir orig (frem (theta - radians angle) (2*pi))
    putStrLn $ printf "right %d: %s -> %s" (toInteger $ truncate angle) (show orig) (show next)
    return next

left :: Double -> Program
left = right . ((-1.0) *)

penup :: Program
penup ts = do
  putStrLn "penup"
  mapM (\t -> return $ setPen t False) ts

pendown :: Program
pendown ts = do
  putStrLn "pendown"
  mapM (\t -> return $ setPen t True) ts

color :: Double -> Double -> Double -> Program
color r g b ts = do
  putStrLn "color"
  mapM (\t -> return $ setCol t (r, g, b)) ts

colorBy :: ([Turtle] -> Color) -> Program
colorBy fn ts = let (r, g, b) = fn ts
                in color r g b ts

(<|>) :: Program -> Program -> Program
(prog1 <|> prog2) ts = do
  ts1 <- prog1 ts
  ts2 <- prog2 ts
  return $ ts1 ++ ts2

times :: Int -> Program -> Program
times n prog ts
  | n == 0 = return ts
  | n > 0  = prog ts >>= times (n-1) prog

forever :: Program -> Program
forever prog ts =
  prog ts >>= forever prog

-- | auxiliary functions
-- * degree to radian conversion
radians :: Double -> Double
radians d = d * pi / 180.0

-- * radian to degree conversion
degrees :: Double -> Double
degrees r = r * 180.0 / pi

-- * double remainder
frem :: Double -> Double -> Double
frem x y = x - (y * (fromIntegral $ floor (x/y)))
