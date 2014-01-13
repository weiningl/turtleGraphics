-- | Turtle data types
module Turtle.Representation (
  -- * Type definitions
  Position
  , Angle
  , Color

  -- * Turtle state and initiation
  , Turtle (..)
  , newTurtle

  -- * Floating point computation
  , degrees
  , radians
  , frem

  -- * Single turtle program
  , Program
  , forward
  , right

  , penup
  , pendown
  , color

  ) where

-- | 2D position.
type Position = (Double, Double)
-- | Orientation, in degree.
type Angle = Double
-- | RGB
type Color = (Double, Double, Double)

-- | Turtle state.
data Turtle = TS { pos :: Position
                 , dir :: Angle
                 , pen :: Bool
                 , col :: Color
                 } deriving (Show)

-- | Create a turtle.
newTurtle :: Turtle
newTurtle = TS { pos = (0.0, 0.0)
               , dir = radians 90.0
               , pen = True
               , col = (1.0, 1.0, 1.0)
               }

-- | Conversion: degrees to radians.
radians :: Double -> Double
radians d = d * pi / 180.0
-- | Conversion: radians to degrees.
degrees :: Double -> Double
degrees r = r * 180.0 / pi

-- | Double remainder.
frem :: Double -> Double -> Double
frem x y = x - (y * (fromIntegral . floor) (x / y))

-- | Single turtle programs
type Program = Turtle -> Turtle
-- | Advance turtle.
forward :: Double -> Program
forward dist t =
  let (x, y) = pos t
      theta = dir t
  in t { pos = (x + dist * cos(theta), y + dist * sin(theta)) }

-- | Rotate right.
right :: Double -> Program
right theta t = t { dir = theta' }
  where theta' = frem (dir t - radians theta) (2*pi)

-- | Show no trace when advance.
penup :: Program
penup t = t { pen = False }

-- | Show trace when advance.
pendown :: Program
pendown t = t { pen = True }

-- | Set trace color.
color :: Double -> Double -> Double -> Program
color r g b t = t { col = (r, g, b) }
