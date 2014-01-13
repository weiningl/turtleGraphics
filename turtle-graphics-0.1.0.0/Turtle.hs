-- | proper module documentation here
-- Turtle graphics domain specific language.

module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
  Program
  , Turtle (..)

  -- * Primitive operations
  -- , forward
  -- , (<|>)
  -- , ...
  , newTurtle

  , identity
  , forward
  , backward
  , right
  , left

  , penup
  , pendown
  , color

  , (>>>)
  , (<|>)

  , times
  , forever

  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...
  , runProg
  , runTextual

  ) where

import Turtle.Program as TurtleImpl
