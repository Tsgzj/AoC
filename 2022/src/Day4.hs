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

main :: IO ()
main = do
  inp <- lines <$> readFile "./../input/Day4.txt"
  print $ length (filter include (map parseLine inp))
