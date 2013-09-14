-- | A simple library for turtle graphics. Shallow embedding.
module Turtle.Shallow
       (
         -- * Turtle graphics program type
         Program
         , Turtle(..)

         -- * Primitive operations
         , newTurtle
         , identity

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

         , (>>>)
         , (<|>)
         , save

         -- * Run functions
         , run
         , runTextual
         ) where

import Text.Printf
import Tree

-- | 2D coordinate.
type Position = (Double, Double)
-- | Angle, in degree.
type Orientation = Double
-- | RGB color.
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
  show (Turtle (x,y) dir pen (r, g, b)) = let
    strs = [printf "Position: (%.2f, %.2f)" x y
           , printf "Direction: %.2f" $ degrees dir
           , printf "Pen: " ++ show pen
           , printf "Color: (%.2f, %.2f, %.2f)" r g b
           ]
    in unwords strs

-- | Turtle graphics program type
type Program = Tree (Turtle -> Turtle)

-- Primitive operations
-- | Create a turtle.
newTurtle :: Turtle
newTurtle = Turtle { pos = (0.0, 0.0)
                   , dir = radians 90.0
                   , pen = True
                   , col = (1.0, 1.0, 1.0)
                   }

-- | Do nothing
identity :: Program
identity = leaf $ \t -> t

-- | Advance turtle.
forward :: Double -> Program
forward dist = leaf $ \t ->
  let (x1, y1) = pos t
      theta = dir t
  in setPos t (x1 + dist * cos(theta), y1 + dist * sin(theta))

-- | Advance in opposite direction.
backward :: Double -> Program
backward dist = forward (-dist)

-- | Rotate right.
right :: Double -> Program
right theta = leaf $ \t ->
  setDir t $ frem (dir t - radians theta) (2*pi)

-- | Rotate left.
left :: Double -> Program
left theta = right (-theta)

-- | Show no trace when advance.
penup :: Program
penup = leaf $ (flip setPen) False

-- | Show turtle trace when advance.
pendown :: Program
pendown = leaf $ (flip setPen) True

-- | Set trace color.
color :: Double -> Double -> Double -> Program
color r g b = leaf $ (flip setCol) (r, g, b)

-- | Set trace color based on turtle state.
colorBy :: (Turtle -> Color) -> Program
colorBy fn = leaf $ \t -> setCol t (fn t)

-- | Serial program combination.
(>>>) :: Program -> Program -> Program
prog1 >>> prog2 = case prog1 of
  End -> prog2
  Terminal -> Terminal
  Fork a l End -> Fork a (l >>> prog2) Terminal
  Fork a l r -> Fork a (l >>> prog2) (r >>> prog2)

-- | Parallel program combination.
(<|>) :: Program -> Program -> Program
prog1 <|> prog2 = Fork (\t -> t) prog1 prog2

-- | Save current state/program. It is execute after later programs are done.
save :: Program -> Program
save prog = identity <|> (freeze prog)

-- | Repeat program.
times :: Int -> Program -> Program
times n prog
  | n == 1 = prog
  | n > 1 = prog >>> times (n-1) prog

-- | Infinite repeat.
forever :: Program -> Program
forever prog = prog >>> forever prog

-- | auxiliary functions

-- | Degree to radian conversion.
radians :: Double -> Double
radians d = d * pi / 180.0

-- | Radian to degree conversion.
degrees :: Double -> Double
degrees r = r * 180.0 / pi

-- | Double remainder.
frem :: Double -> Double -> Double
frem x y = x - (y * (fromIntegral $ floor (x/y)))

-- | run program
run :: Program -> Turtle -> [(Turtle, Turtle)]
run End _ = []
run Terminal _ = []
run (Fork f l r) init = let next = f init
                        in ((init, next) : (run l next)) ++ (run r next)

-- | Text output only
runTextual :: Program -> IO ()
runTextual prog = let init = newTurtle
                      lines = map show $ init : (map snd $ run prog init)
                  in putStrLn $ unlines lines
