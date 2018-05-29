{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SkiTypes where

import Data.Proxy

data I
data K
data S
data A
data B

data Leaf x
data Node left right

class Reduce combs result  | combs -> result where
  reduction :: Proxy combs -> Proxy result
  reduction Proxy = Proxy

-- if there's a non-ski symbol on the leftmost side of the tree, leave it and reduce
-- the rest of the tree
instance (Reduce ast innerResult) => Reduce (Node (Leaf A) ast) (Node (Leaf A) innerResult)
instance (Reduce ast innerResult) => Reduce (Node (Leaf B) ast) (Node (Leaf B) innerResult)

-- the catch-all recursive step—if the left-most term reduces, reduce it before applying it to
-- the rest of the expression
instance {-# OVERLAPS #-} (Reduce (Node x y) r, Reduce (Node a b) x) => Reduce (Node (Node a b) y) r

-- Symbols should reduce to themselves
instance Reduce (Leaf x) (Leaf x)

-- I recursion
instance Reduce x y => Reduce (Node (Leaf I) x) y

-- K recursion
instance Reduce (Node (Leaf K) a) (Node (Leaf K) a)
instance Reduce x y => Reduce (Node (Node (Leaf K) x) z) y

-- S recursion
instance Reduce (Node (Leaf S) a) (Node (Leaf S) a)
instance Reduce (Node (Node (Leaf S) a) b) (Node (Node (Leaf S) a) b)
instance Reduce (Node (Node a c) (Node b c)) result => Reduce (Node (Node (Node (Leaf S) a) b) c) result


-- https://en.wikipedia.org/wiki/SKI_combinator_calculus#Self-application_and_recursion
-- SII(SII) infinite loops
-- :t reduction (Proxy :: (Proxy (Node (Node (Node (Leaf S) (Leaf I)) (Leaf I)) (Node (Node (Leaf S) (Leaf I)) (Leaf I)))))

-- example for understanding computation
-- :t reduction (Proxy :: (Proxy (Node (Node (Node (Node (Leaf I) (Leaf S)) (Leaf K)) (Leaf S)) (Node (Leaf I) (Leaf K)))))

-- start ghci without a type reduction limit -> % ghci skiType2 -freduction-depth=0

-- reversal: S(K(SI))Kαβ
-- :t reduction (Proxy :: (Proxy
--   (Node
--     (Node
--       (Node
--         (Node
--           (Leaf S)
--           (Node
--             (Leaf K)
--             (Node
--               (Leaf S)
--               (Leaf I))))
--         (Leaf K))
--       (Leaf A))
--     (Leaf B))))

-- :t reduction (Proxy :: (Proxy (Node (Node (Node (Node (Leaf S) (Node (Leaf K) (Node (Leaf S) (Leaf I)))) (Leaf K)) (Leaf A)) (Leaf B))))
