module ListCombine where

import Data.List (group, groupBy, sort, sortBy)
import Control.Applicative (Applicative, pure, liftA2)

import Combine(combineR, combinationsL)

listCombine = combineR (:) []
listCombinations = combinationsL (flip (:)) []

groupPermutations :: Ord b => (a -> b) -> [a] -> [[(a, b)]]
groupPermutations f = let check a b = (snd a == snd b, compare (snd a) (snd b)) in
    groupBy ((fst .) . check) .
    sortBy ((snd .) . check) .
    map (\x -> (x, f x))

likeTerms :: Ord b => ([a] -> b) -> [[a]] -> [Int]
likeTerms = ((map length . group . sort) .) . map

slice :: Ord b => ([a] -> b) -> [[a]] -> [Int]
slice f = likeTerms f . listCombine

struct :: Ord b => ([a] -> b) -> [[a]] -> [[Int]]
struct f = map (likeTerms f) . listCombinations
