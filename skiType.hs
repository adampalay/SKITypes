{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module SkiTypes where

import Data.Proxy

data I
data K
data S
data A
data B

data left $: right

class Reduce combs result  | combs -> result where
  reduction :: Proxy combs -> Proxy result
  reduction Proxy = Proxy

-- if there's a non-ski symbol on the leftmost side of the tree, leave it and reduce
-- the rest of the tree
instance (Reduce ast innerResult) => Reduce (A $: ast) (A $: innerResult)
instance (Reduce ast innerResult) => Reduce (B $: ast) (B $: innerResult)

-- the catch-all recursive step—if the left-most term reduces, reduce it before applying it to
-- the rest of the expression
instance {-# OVERLAPS #-} (Reduce (x $: y) r, Reduce (a $: b) x) => Reduce (a $: b $: y) r

-- Symbols should reduce to themselves
instance Reduce A A
instance Reduce B B
instance Reduce S S
instance Reduce K K
instance Reduce I I

-- I recursion
instance Reduce x y => Reduce (I $: x) y

-- K recursion
instance Reduce (K $: a) (K $: a)
instance Reduce x y => Reduce (K $: x $: z) y

-- S recursion
instance Reduce (S $: a) (S $: a)
instance Reduce (S $: a $: b) (S $: a $: b)
instance Reduce (a $: c $: (b $: c)) result => Reduce (S $: a $: b $: c) result


-- https://en.wikipedia.org/wiki/SKI_combinator_calculus#Self-application_and_recursion
-- SII(SII) infinite loops
-- :t reduction (Proxy :: (Proxy (S $: I $: I $: (S $: I $: I))))

-- example for understanding computation
-- :t reduction (Proxy :: (Proxy (I $: S $: K $: S $: (I $: K))))

-- start ghci without a type reduction limit -> % ghci skiType2 -freduction-depth=0

-- reversal: S(K(SI))Kαβ
-- :t reduction (Proxy :: (Proxy (S $: (K $: (S $: I)) $: K $: A $: B)))
