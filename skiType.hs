{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SkiTypes where

import Data.Proxy

data I
data K
data S

data Leaf x
data Node left right

class Reduce combs result  | combs -> result where
  reduction :: Proxy combs -> Proxy result
  reduction Proxy = Proxy
instance Reduce (Leaf I) (Leaf I)
instance Reduce x y => Reduce (Node (Leaf I) x) y

instance Reduce (Leaf K) (Leaf K)
instance Reduce (Node (Leaf K) a) (Node (Leaf K) a)
instance Reduce x y => Reduce (Node (Node (Leaf K) x) z) y

instance Reduce (Leaf S) (Leaf S)
instance Reduce (Node (Leaf S) a) (Node (Leaf S) a)
instance Reduce (Node (Node (Leaf S) a) b) (Node (Node (Leaf S) a) b)
instance Reduce (Node (Node a c) (Node b c)) result => Reduce (Node (Node (Node (Leaf S) a) b) c) result

-- instance (Reduce (Node x y) r, Reduce (Node a b) x) => Reduce (Node (Node a b) y) r
-- "pattern match on all other instances"
instance (Reduce (Node x y) r, Reduce (Node (Leaf I) b) x) => Reduce (Node (Node (Leaf I) b) y) r
instance (Reduce (Node (Node x y) z) r, Reduce (Node (Leaf K) b) x) => Reduce (Node (Node (Node (Leaf K) b) y) z) r
instance (Reduce (Node (Node x y) z) r, Reduce (Node (Leaf I) b) x) => Reduce (Node (Node (Node (Leaf I) b) y) z) r

instance (Reduce (Node (Node (Node x t) y) z) r, Reduce (Node (Leaf S) b) x) => Reduce (Node (Node (Node (Node (Leaf S) b) t) y) z) r
instance (Reduce (Node (Node (Node x t) y) z) r, Reduce (Node (Leaf K) b) x) => Reduce (Node (Node (Node (Node (Leaf K) b) t) y) z) r
instance (Reduce (Node (Node (Node x t) y) z) r, Reduce (Node (Leaf I) b) x) => Reduce (Node (Node (Node (Node (Leaf I) b) t) y) z) r
instance (Reduce (Node (Node (Node x t) y) z) r, Reduce (Node (Node a b) c) x) => Reduce (Node (Node (Node (Node (Node a b) c) t) y) z) r


-- https://en.wikipedia.org/wiki/SKI_combinator_calculus#Self-application_and_recursion
-- SII(SII) infinite loops
-- :t reduction (Proxy :: (Proxy (Node (Node (Node (Leaf S) (Leaf I)) (Leaf I)) (Node (Node (Leaf S) (Leaf I)) (Leaf I)))))

-- example for understanding computation
-- :t reduction (Proxy :: (Proxy (Node (Node (Node (Node (Leaf I) (Leaf S)) (Leaf K)) (Leaf S)) (Node (Leaf I) (Leaf K)))))

-- start ghci without a type reduction limit -> % ghci skiType2 -freduction-depth=0
