-- | A simple library for turtle graphics. Deep embedding.
module Turtle.Deep
       (
         -- * Turtle graphics program type
         Program

         -- * Primitive operations

         -- * Combinators
         , (>>>)
         , (<|>)

       ) where

import Turtle.State

-- Turtle graphics language type definition
data Prog a = Primitive (a -> a)
            | Chain [Prog a]
            | Fork (Prog a) (Prog a)
type Program = Prog Turtle

-- | Serial combinator.
(>>>) :: Program -> Program -> Program
(Chain xs1) >>> (Chain xs2) = Chain (xs1 ++ xs2)
(Chain xs1) >>> prog2 = Chain (xs1 ++ [prog2])
prog1 >>> (Chain xs2) = Chain (prog1 : xs2)
prog1 >>> prog2 = Chain [prog1, prog2]

-- | Parallel combinator.
(<|>) :: Program -> Program -> Program
prog1 <|> prog2 = Fork prog1 prog2
