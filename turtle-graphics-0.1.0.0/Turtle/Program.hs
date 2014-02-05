-- | Turtle graphics program
module Turtle.Program (
  -- * Data types
  Program
  , Turtle (..)

  -- * Program combinators
  , (>>>)
  , (<|>)
  , times
  , forever

  -- * Turtle programs
  , T.newTurtle

  , identity
  , forward
  , backward
  , right
  , left
  , penup
  , pendown
  , color

  -- * Run program
  , runProg
  , runTextual

  ) where

import Control.Monad.Writer hiding (forever)
import Data.Monoid

import qualified Turtle.Representation as T
import Turtle.Representation (Turtle)

-- | Turtle program
type Prog w a = [a] -> Writer w [a]

-- | Serial program combination.
(>>>) :: (Monoid w) => Prog w a -> Prog w a -> Prog w a
(prog1 >>> prog2) x = prog1 x >>= prog2

-- | Parallel program combination.
(<|>) :: (Monoid w) => Prog w a -> Prog w a -> Prog w a
(prog1 <|> prog2) x = writer $ let
  (a1, w1) = runWriter (prog1 x)
  (a2, w2) = runWriter (prog2 x)
  in (a1 ++ a2, w1 `mappend` w2)

-- | Repeat program.
times :: (Monoid w) => Int -> Prog w a -> Prog w a
times n prog
  | n == 1 = prog
  | n > 1 = prog >>> times (n - 1) prog

-- | Repeat program forever.
forever :: (Monoid w) => Prog w a -> Prog w a
forever prog = prog >>> forever prog

-- | Map single state program to multiple state program
liftProgram :: (a -> a) -> Prog [(a, a)] a
liftProgram f xs = do tell ws
                      return xs'
  where xs' = fmap f xs
        ws = zip xs xs'

type Program = Prog [(Turtle, Turtle)] Turtle

identity :: Program
identity = liftProgram id

forward, backward :: Double -> Program
forward dist = liftProgram (T.forward dist)
backward dist = forward (-dist)

right, left :: Double -> Program
right theta = liftProgram (T.right theta)
left theta = right (-theta)

penup, pendown :: Program
penup = liftProgram T.penup
pendown = liftProgram T.pendown

color :: Double -> Double -> Double -> Program
color r g b = liftProgram (T.color r g b)

-- | Run program.
runProg :: Program -> [(Turtle, Turtle)]
runProg prog = execWriter $ prog [T.newTurtle]

-- | Textual output of a program
runTextual :: Program -> IO ()
runTextual prog = putStrLn $ unlines lines
  where trace = runProg prog
        lines = map show trace
