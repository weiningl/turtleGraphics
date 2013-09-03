module Main where

import Control.Monad
import TurtleGraphics
import Turtle

circular :: Int -> Double -> Bool -> Program -> Program
circular n radius sneak prog turtles =
  return turtles
  >>= penup
  >>= forward radius
  >>= right initAngle
  >>= pendown
  >>= times n (fn prog)
  >>= penup
  >>= left initAngle
  >>= backward radius
  >>= pendown
  where angle = 360.0 / (fromIntegral n)
        initAngle = 90.0 - angle / 2
        sideLength = radius * sin (2 * pi / (fromIntegral n))
        fn prog = \ts ->
          (if (sneak) then penup ts else pendown ts)
          >>= right angle
          >>= forward sideLength
          >>= pendown
          >>= prog

circle radius = circular 360 radius False (\ts -> return ts)
flower n size radius = circular n size True $ (\ts ->
                                                colorByPosition 0.4 2.0 ts
                                                >>= circle radius)

spiral size angle turtles =
  let maxSize = 0.4
      stepSize = 0.01
  in if (size > maxSize) then return turtles
     else return turtles
          >>= colorByPosition (size / maxSize) 1.2
          >>= (\ts -> if (size > 0.2 * maxSize && size < 0.4 * maxSize) then penup ts
                      else pendown ts)
          >>= forward size
          >>= right angle
          >>= spiral (size + stepSize) angle

colorByPosition r amp = colorBy (\ts -> let (x, y) = (pos . head) ts
                                        in (r, amp * abs x, amp * abs y))

goLeft dist turtles =
  return turtles
  >>= penup
  >>= left 90
  >>= forward dist
  >>= pendown

goRight dist turtles =
  return turtles
  >>= penup
  >>= right 90
  >>= forward dist
  >>= pendown

doubleSpiral turtles =
  return turtles
  >>= (goLeft 0.4 <|> goRight 0.4)
  >>= spiral 0 91

quadSpiral turtles =
  return turtles
  >>= (goLeft 0.5 <|> goRight 0.5)
  >>= doubleSpiral

main = runGraphical $ (flower 12 0.4 0.5 <|> quadSpiral)
