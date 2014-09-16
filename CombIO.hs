module Main (main) where

import System.Environment (getArgs)
import Control.Monad ((>=>))

import ListCombine (listCombine, slice, groupPermutations)


options :: [(String, [String] -> IO ())]
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

fcomb :: String -> IO ()
fcomb = putStr . unlines . map unwords . listCombine . map words . lines

lcomb :: [String] -> IO ()
lcomb = putStrLn . unwords . listCombine

enumSum :: Enum a => [a] -> Int
enumSum = sum . map fromEnum

lslice :: Enum a => [[a]] -> IO ()
lslice = putStrLn . unwords . map show . (slice enumSum)

lgroup :: [String] -> IO ()
lgroup = putStr .
    unlines . map (\l -> (show $ length l) ++ "\t" ++ unwords l) . -- format
    fst . unzip . map unzip . -- extract
    groupPermutations enumSum . listCombine -- generate

combIO :: [String] -> IO ()
combIO [] = getContents >>= fcomb
combIO (('-':flag):args) = dispatch $ lookup flag options
    where dispatch Nothing = putStrLn help
	  dispatch (Just opt) = opt args
combIO fNs = sequence_ $ map (readFile >=> fcomb) fNs

main = getArgs >>= combIO
