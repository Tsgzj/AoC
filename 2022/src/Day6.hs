module Day6 where

import Data.List

common :: Int -> String -> Int
common n input = n + fromJust (findIndex unique $ sublists n input)

part1 :: String -> Int
part1 = common 4

part2 :: String -> Int
part2 = common 14

fromJust :: Maybe a -> a
fromJust (Just x) = x

sublists :: Int -> [a] -> [[a]]
sublists n xs = filter (\x -> length x == n) $ contiguous n xs
  where
    contiguous _ [] = []
    contiguous n (x:xs) =
      take n (x:xs) : contiguous n xs

unique :: String -> Bool
unique s = nub s == s

main :: IO ()
main = do
  input <- readFile "./../input/Day6.txt"
  print $ part1 input
  print $ part2 input
