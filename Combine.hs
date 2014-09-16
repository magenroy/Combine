module Combine where

import Prelude hiding (foldr, foldl, foldr1, foldl1)

import Data.Monoid (Monoid, mempty, mappend)
import Data.Foldable (Foldable, foldr, foldl, foldr', foldl', foldr1, foldl1)
import Control.Applicative (Applicative, pure, liftA2)

-- equiv of fold
combine :: (Foldable f, Applicative a, Monoid m) => f (a m) -> a m
combine = foldr (liftA2 mappend) (pure mempty)

-- equiv of foldr
combineR :: (Foldable f, Applicative a) => (v -> w -> w) -> w -> f (a v) -> a w
combineR f = foldr (liftA2 f) . pure

-- equiv of foldl
combineL :: (Foldable f, Applicative a) => (w -> v -> w) -> w -> f (a v) -> a w
combineL f = foldl (liftA2 f) . pure


-- combine with no base case (for non-empty structures)
combine1 :: (Foldable f, Applicative a, Monoid m) => f (a m) -> a m
combine1 = foldr1 (liftA2 mappend)

-- equiv of foldr1
combineR1 :: (Foldable f, Applicative a) => (v -> v -> v) -> f (a v) -> a v
combineR1 f = foldr1 (liftA2 f)

-- equiv of foldl1
combineL1 :: (Foldable f, Applicative a) => (v -> v -> v) -> f (a v) -> a v
combineL1 f = foldl1 (liftA2 f)


-- strict version of "combine"
combine' :: (Foldable f, Applicative a, Monoid m) => f (a m) -> a m
combine' = foldr' (liftA2 mappend) (pure mempty)

-- equiv of foldr'
combineR' :: (Foldable f, Applicative a) => (v -> w -> w) -> w -> f (a v) -> a w
combineR' f = foldr' (liftA2 f) . pure

-- equiv of foldl'
combineL' :: (Foldable f, Applicative a) => (w -> v -> w) -> w -> f (a v) -> a w
combineL' f = foldl' (liftA2 f) . pure



-- scan version of combine
combinations :: (Applicative a, Monoid m) => [a m] -> [a m]
combinations = scanr (liftA2 mappend) (pure mempty)

-- equiv of scanr
combinationsR :: Applicative a => (v -> w -> w) -> w -> [a v] -> [a w]
combinationsR f = scanr (liftA2 f) . pure

-- equiv of scanl
combinationsL :: Applicative a => (w -> v -> w) -> w -> [a v] -> [a w]
combinationsL f = scanl (liftA2 f) . pure


-- equiv of scanr
combinationsR1 :: Applicative a => (v -> v -> v) -> [a v] -> [a v]
combinationsR1 f = scanr1 (liftA2 f)

-- equiv of scanl
combinationsL1 :: Applicative a => (v -> v -> v) -> [a v] -> [a v]
combinationsL1 f = scanl1 (liftA2 f)

-- SO MUCH BOILERPLATE!!!
