{-# LANGUAGE TemplateHaskell #-}

-- | Binary tree
module Tree
       (
         -- * Tree data type
         Tree(..)

         -- * Tree operations
         , leaf
         , freeze
       ) where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All

data Tree a = End | Terminal | Fork a (Tree a) (Tree a) deriving (Show)

instance (Eq a) => Eq (Tree a) where
  End == End = True
  End == Terminal = True
  Terminal == End = True
  Terminal == Terminal = True
  (Fork a1 l1 r1) == (Fork a2 l2 r2) = (a1 == a2) && (l1 == l2) && (r1 == r2)
  _ == _ = False

instance Functor Tree where
  fmap f End = End
  fmap f Terminal = Terminal
  fmap f (Fork a l r) = Fork (f a) (fmap f l) (fmap f r)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbEnd = frequency [ (4, return End)
                   , (1, return Terminal) ]

arbTree :: (Arbitrary a) => Int -> Gen (Tree a)
arbTree 0 = arbEnd
arbTree n = frequency [ (1, arbEnd)
                      , (4, liftM3 Fork arbitrary
                            (arbTree (n `div` 2)) (arbTree (n `div` 2)))]

-- | create leaf
leaf :: a -> Tree a
leaf x = Fork x End End

-- | stop tree growth
freeze :: Tree a -> Tree a
freeze End = Terminal
freeze Terminal = Terminal
freeze (Fork a l r) = Fork a (freeze l) (freeze r)

prop_frozenTreeHasNoLeafAlive :: Tree a -> Bool
prop_frozenTreeHasNoLeafAlive t = notAlive $ freeze t
  where notAlive End = False
        notAlive Terminal = True
        notAlive (Fork _ l r) = notAlive l && notAlive r

prop_frozenTreePreserves t = (freeze t) == t

runTests = $quickCheckAll
