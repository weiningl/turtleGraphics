module Main where

import Control.Monad
import TurtleGraphics
import Turtle

circular :: Int -> Double -> Bool -> Program -> Program
circular n radius sneak prog =
  penup
  >>> forward radius
  >>> right initAngle
  >>> pendown
  >>> times n (fn prog)
  >>> penup
  >>> left initAngle
  >>> backward radius
  >>> pendown
  where angle = 360.0 / (fromIntegral n)
        initAngle = 90.0 - angle / 2
        sideLength = radius * sin (2 * pi / (fromIntegral n))
        fn prog =
          (if (sneak) then penup else pendown)
          >>> right angle
          >>> forward sideLength
          >>> pendown
          >>> prog

circle radius = circular 360 radius False identity
flower n size radius = circular n size True $ (colorByPosition 0.4 2.0
                                               >>> circle radius)

spiral size angle =
  if (size > maxSize) then identity
  else colorByPosition (size / maxSize) 1.2
       >>> (if (size > 0.2 * maxSize && size < 0.4 * maxSize) then penup
            else pendown)
       >>> forward size
       >>> right angle
       >>> spiral (size + stepSize) angle
  where maxSize = 0.4
        stepSize = 0.01

colorByPosition r amp = colorBy (\t -> let (x, y) = pos t
                                       in (r, amp * abs x, amp * abs y))

goLeft dist =
  penup
  >>> left 90
  >>> forward dist
  >>> pendown

goRight dist =
  penup
  >>> right 90
  >>> forward dist
  >>> pendown

doubleSpiral =
  (goLeft 0.4 <|> goRight 0.4)
  >>> spiral 0 91

quadSpiral =
  (goLeft 0.5 <|> goRight 0.5)
  >>> doubleSpiral

main = runGraphical $ (flower 6 0.4 0.5 <|> quadSpiral)
