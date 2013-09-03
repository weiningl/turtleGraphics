-- | proper module documentation here
-- Turtle graphics domain specific language.

module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
  module TurtleImpl
  -- * Primitive operations
  -- , forward
  -- , (<|>)
  -- , ...

  -- * Derived operations
  -- ...

  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...

  ) where

import Turtle.Shallow as TurtleImpl
