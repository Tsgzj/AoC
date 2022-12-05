module Day1 where

import           Data.List.Split
import           Data.List

input :: IO String
input = readFile "./../input/Day1.txt"

parseInput :: String -> [Int]
parseInput = map sum . map (map read) . splitOn [""] . lines

solve1 :: [Int] -> Int
solve1 = maximum

solve2 :: [Int] -> Int
solve2 = sum . take 3 . sortOn negate

main :: IO ()
main = do
  l <- input
  print $ solve1 (parseInput l)
  print $ solve2 (parseInput l)
