module Day4 where

import           Data.List.Split

parseOn :: Read a =>(String -> [String]) -> (String -> a) -> String -> (a, a)
parseOn fn tx s = (tx (head p), tx (head (tail p)))
  where p = fn s

parsePair :: String -> (Int, Int)
parsePair = parseOn (splitOn "-") read

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = parseOn (splitOn ",") parsePair

include :: ((Int, Int), (Int, Int)) -> Bool
include ((a, b), (c, d)) = a <=c && b >= d ||a >= c && b <= d

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d)) = r - l >= 0
  where l = max a c
        r = min b d 

part1 :: [String] -> Int
part1 = length . filter (include . parseLine)

part2 :: [String] -> Int
part2 = length . filter (overlap . parseLine)

main :: IO ()
main = do
  input <- lines <$> readFile "./../input/Day4.txt"
  print $ part1 input
  print $ part2 input
