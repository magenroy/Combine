module Combinations where

import Data.List (group, groupBy, sort, sortBy)
import Control.Applicative (Applicative, pure, liftA2)

collectPermutations :: Ord b => ([a] -> b) -> [[a]] -> [[b]]
collectPermutations = ((group . sort) .) . map

groupPermutations :: Ord b => (a -> b) -> [a] -> [[(a, b)]]
groupPermutations f = let check a b = (snd a == snd b, compare (snd a) (snd b)) in
    groupBy ((fst .) . check) .
    sortBy ((snd .) . check) .
    map (\x -> (x, f x))

likeTerms :: Ord b => ([a] -> b) -> [[a]] -> [Int]
likeTerms = (map length .) . collectPermutations
likeTerms' f = (map length .) . groupPermutations $ f -- why is this so much slower? (are fst and snd really that slow?)

slice :: Ord b => ([a] -> b) -> [[a]] -> [Int]
slice f = likeTerms f . combine

struct :: Ord b => ([a] -> b) -> [[a]] -> [[Int]]
struct f = map (likeTerms f) . combinations
struct' f = map (likeTerms' f) . combinations
