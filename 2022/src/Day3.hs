module Day3 where

import Data.Char (ord)
import Data.List

split :: [a] -> ([a], [a])
split l = splitAt ((length l + 1) `div` 2) l

commonChar :: String -> Char
commonChar = head . uncurry intersect . split

commonChar2 :: [String] -> Char
commonChar2 = head . foldl1 intersect

groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n l
    | n > 0 = take n l : groupn n (drop n l)
    | otherwise = error "Invalid"

charScore :: Char -> Int
charScore c | c >= 'a' = ord c - 96
            | otherwise = ord c - 38

main :: IO ()
main = do
  l <- lines <$> readFile "./../input/Day3.txt"
  print $ sum (map (charScore . commonChar) l)
  print $ sum (map (charScore . commonChar2) (groupn 3 l))
