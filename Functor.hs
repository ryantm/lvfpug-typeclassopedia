{-# LANGUAGE OverloadedStrings #-}

module Functor where

import Prelude hiding (Functor, fmap)

-- $setup
-- >>> import Test.QuickCheck.Function


class Functor f where
  fmap :: (a -> b) -> f a -> f b


-- | Functor instance for Either e
--
-- >>> fmap (<5) (Left "error")
-- Left "error"
--
-- >>> fmap (<5) (Right 1)
-- Right True
--
-- Identity law: fmap id = id
--
-- prop> fmap id xs == id (xs :: Either String Int)
--
-- >>> fmap id (Right 1)
-- Right 1
--
-- >>> fmap id (Left "hi")
-- Left "hi"
--
-- Associativity law:
-- fmap (g . h) = (fmap g) . (fmap h)
--
-- >>> let e = Left "error"
-- >>> let g = show :: Bool -> String
-- >>> let h = (<5)
-- >>> fmap (g . h) e == ((fmap g) . (fmap h)) e
-- True
--
-- >>> let e = Right 5
-- >>> let g = show :: Bool -> String
-- >>> let h = (<5)
-- >>> fmap (g . h) e == ((fmap g) . (fmap h)) e
-- True
--
instance Functor (Either e) where
  fmap f (Left e) = Left e
  fmap f (Right a) = Right (f a)


-- | Functor instance for ((->) e)
--
-- >>> (fmap show (<5)) 4
-- "True"
--
-- Identity law:
--
-- >>> let input = 10
-- >>> (fmap id (<5)) input == id (input < 5)
-- True
--
-- >>> let input = 4
-- >>> (fmap id (<5)) input == id (input < 5)
-- True
--
-- Associativity law:
-- fmap (g . h) = (fmap g) . (fmap h)
--
-- >>> let input = 10
-- >>> let g = show :: Bool -> String
-- >>> let h = (<5)
-- >>> let i = (+4)
-- >>> (fmap (g . h) i) input == (((fmap g) . (fmap h)) i) input
-- True
--
-- >>> let input = 0
-- >>> let g = show :: Bool -> String
-- >>> let h = (<5)
-- >>> let i = (+4)
-- >>> (fmap (g . h) i) input == (((fmap g) . (fmap h)) i) input
-- True
instance Functor ((->) e) where
  fmap f g = f . g


--((,) e) and for Pair, defined as
--data Pair a = Pair a a

instance Functor ((,) e) where
  fmap f (e, a) = (e, f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair b c) = Pair (f b) (f c)


data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor ITree where
  fmap f (Leaf g) = (Leaf (f . g))
  fmap f (Node ts) = Node (fmap (fmap f) ts)
