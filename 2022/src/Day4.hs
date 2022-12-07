module Day4 where

import           Data.List.Extra hiding (splitOn)
import Data.List.Split (splitOn)
import Data.Char
import Data.Function

parseOn :: Read a =>(String -> [String]) -> (String -> a) -> String -> (a, a)
parseOn fn tx s = (tx (head p), tx (head (tail p)))
  where p = fn s

parsePair :: String -> (Int, Int)
parsePair = parseOn (splitOn "-") read

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = parseOn (splitOn ",") parsePair

-- from reddit discussion
-- https://www.reddit.com/r/haskell/comments/zc108l/comment/iyuayd4/?utm_source=share&utm_medium=web2x&context=3
parseLine2 :: String -> ((Int, Int), (Int, Int))
parseLine2 line = ((read lx, read hx), (read ly, read hy))
  where
    lx : _ : hx : _ : ly : _ : hy : _ = groupBy ((==) `on` isDigit) line

include :: ((Int, Int), (Int, Int)) -> Bool
include ((a, b), (c, d)) = a <=c && b >= d ||a >= c && b <= d

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d)) = not $ a > d || c > b

part1 :: [String] -> Int
part1 = length . filter (include . parseLine)

part2 :: [String] -> Int
part2 = length . filter (overlap . parseLine)

main :: IO ()
main = do
  input <- lines <$> readFile "./../input/Day4.txt"
  print $ part1 input
  print $ part2 input
