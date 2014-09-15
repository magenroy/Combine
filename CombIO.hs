module Main (main) where

import System.Environment (getArgs)
import Control.Monad ((>=>))

import Combinations (combinations, slice, groupPermutations)
import Combine (combineR)

-- make structure generator
--  requires finding way to properly group the strings

options =
    ("s", lcomb):("-strings", lcomb):
    ("t", lslice):("-terms", lslice):
    ("g", lgroup):("-group", lgroup):
    []


help :: String
help = "Finds all combinations of elements in a list of lists"
    ++ "\n USAGE: combine [FLAG] [ARGUMENTS]"
    ++ "\n\t run with no arguments to use standard input (list of lines containing lists of words)"
    ++ "\n\t run with filenames and no options to use the contents of that file (list of lines containing lists of words)"
    ++ "\n OPTIONS"
    ++ "\n\t -s --strings \t combine argument strings"
    ++ "\n\t -t --terms \t count like strings resulting from combining arguments (strings are compared based on the sum of the numerical representations of their characters)"
    ++ "\n\t -g --group \t group like terms"

comb = combineR (:) []

fcomb :: String -> IO ()
fcomb = putStr . unlines . map unwords . comb . map words . lines

lcomb :: [String] -> IO ()
lcomb = putStrLn . unwords . comb

enumSum :: Enum a => [a] -> Int
enumSum = sum . map fromEnum

lslice :: Enum a => [[a]] -> IO ()
lslice = putStrLn . unwords . map show . (slice enumSum)

lgroup = putStr . unlines . map (unwords) . fst . unzip . map unzip . groupPermutations enumSum . comb -- should indicate how many elements on each line

combIO :: [String] -> IO ()
combIO [] = getContents >>= fcomb
combIO (('-':flag):args) = dispatch $ lookup flag options
    where dispatch Nothing = putStrLn help
	  dispatch (Just opt) = opt args
combIO fNs = sequence_ $ map (readFile >=> fcomb) fNs

main = getArgs >>= combIO
