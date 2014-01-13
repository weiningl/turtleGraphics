-- | Representation of a turtle
module Turtle.State where

import Text.Printf

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

-- | Create a turtle.
newTurtle :: Turtle
newTurtle = Turtle { pos = (0.0, 0.0)
                   , dir = radians 90.0
                   , pen = True
                   , col = (1.0, 1.0, 1.0)
                   }

-- | Degree to radian conversion.
radians :: Double -> Double
radians d = d * pi / 180.0

-- | Radian to degree conversion.
degrees :: Double -> Double
degrees r = r * 180.0 / pi

-- | Double remainder.
frem :: Double -> Double -> Double
frem x y = x - (y * (fromIntegral $ floor (x/y)))
