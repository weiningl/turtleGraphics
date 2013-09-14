-- | Binary tree
module Tree
       (
         -- * Tree data type
         Tree(..)

         -- * Tree operations
         , leaf
         , freeze
       ) where

data Tree a = End | Terminal | Fork a (Tree a) (Tree a)

instance Functor Tree where
  fmap f End = End
  fmap f Terminal = Terminal
  fmap f (Fork a l r) = Fork (f a) (fmap f l) (fmap f r)

-- | create leaf
leaf :: a -> Tree a
leaf x = Fork x End End

-- | stop tree growth
freeze :: Tree a -> Tree a
freeze End = Terminal
freeze Terminal = Terminal
freeze (Fork a l r) = Fork a (freeze l) (freeze r)
