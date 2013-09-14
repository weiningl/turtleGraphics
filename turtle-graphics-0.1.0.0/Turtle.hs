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
  , right

  , penup
  , pendown
  , color

  , (>>>)
  , (<|>)
  -- * Derived operations
  -- ...
  , backward
  , left
  , colorBy
  , times
  , forever
  , save
  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...
  , run
  , runTextual

  ) where

import Turtle.Shallow as TurtleImpl
