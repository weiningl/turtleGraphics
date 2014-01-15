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
import Control.Monad.State hiding (forever)
import Data.Monoid

import qualified Turtle.Representation as T
import Turtle.Representation (Turtle)

-- | Turtle program
type Prog s a = WriterT a (State [s]) a

-- | Serial program combination.
(>>>) :: (Monoid a) => Prog s a -> Prog s a -> Prog s a
prog1 >>> prog2 = do prog1; prog2

-- | Parallel program combination.
(<|>) :: (Monoid a) => Prog s a -> Prog s a -> Prog s a
prog1 <|> prog2 = WriterT $ state $ \s ->
  let ((a1, w1), s1) = (runState . runWriterT) prog1 s
      ((a2, w2), s2) = (runState . runWriterT) prog2 s
  in ((a2, w1 `mappend` w2), s1 ++ s2)

-- | Repeat program.
times :: (Monoid a) => Int -> Prog s a -> Prog s a
times n prog
  | n == 1 = prog
  | n > 1 = prog >>> times (n - 1) prog

-- | Repeat program forever.
forever :: (Monoid a) => Prog s a -> Prog s a
forever prog = prog >>> forever prog

-- | Map single state program to multiple state program
liftProgram :: (s -> s) -> Prog s [(s, s)]
liftProgram f = WriterT $ state $ \xs ->
  let xs' = fmap f xs
      ws' = zip xs xs'
  in ((ws', ws'), xs')

type Program = Prog Turtle [(Turtle, Turtle)]

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
runProg prog = (evalState . execWriterT) prog [T.newTurtle]

-- | Textual output of a program
runTextual :: Program -> IO ()
runTextual prog = putStrLn $ unlines lines
  where trace = runProg prog
        lines = map show trace
