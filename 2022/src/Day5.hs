module Day5 where

import           Data.List.Split
import GHC (XBindStmtTc(xbstc_bindOp))

data Move = Move Int Int Int


crates =
    [['V', 'C', 'D', 'R', 'Z', 'G', 'B', 'W'],
     ['G', 'W', 'F', 'C', 'B', 'S', 'T', 'V'],
     ['C', 'B', 'S', 'N', 'W'],
     ['Q', 'G', 'M', 'N', 'J', 'V', 'C', 'P'],
     ['T', 'S', 'L', 'F', 'D', 'H', 'B'],
     ['J', 'V', 'T', 'W', 'M', 'N'],
     ['P', 'F', 'L', 'C', 'S', 'T', 'G'],
     ['B', 'D', 'Z'],
     ['M', 'N', 'Z', 'W']
    ]

pMove :: String -> Move
pMove s = Move (read a) (read b) (read c) 
  where
    [a,b,c] = splitOn " " s

topOfCrates :: [[Char]] -> String
topOfCrates = map last'
  where
    last' [] = ' '
    last' a = last a

switch :: [[Char]] -> Int -> [Char] -> [[Char]]
switch [] _ _ = []
switch (_ : xs) 0 a = a : xs
switch (x : xs) n a = x : switch xs (n - 1) a

applyMove :: ([Char] -> [Char]) -> [[Char]] -> Move -> [[Char]]
applyMove f c (Move n from to)= switch (switch c (from - 1) nfrom) (to - 1) nto
  where
    t = f . take n . reverse $ c !! (from - 1)
    nfrom = reverse . drop n . reverse $ c !! (from - 1)
    nto = c !! (to - 1) ++ t

part1 :: [Move] -> [[Char]]
part1 = foldl (applyMove id) crates

part2 :: [Move] -> [[Char]]
part2 = foldl (applyMove reverse) crates 

main :: IO ()
main = do
  l <- lines <$> readFile "./../input/Day5.txt"
  print $ topOfCrates (part1 (map pMove l))
  print $ topOfCrates (part2 (map pMove l))

