module Day8 where

import           Data.List

data Tree = Tree Int Bool
  deriving (Show, Eq)
type Forest = [[Tree]]

treeP :: Char -> Tree
treeP n = Tree (read [n]) False

height :: Tree -> Int
height (Tree h _) = h

isVisible :: Tree -> Bool
isVisible (Tree _ v) = v

-- set visibility from 1 direction
setForestVisibility :: Forest -> Forest
setForestVisibility = fmap setRowVisibility

setRowVisibility :: [Tree] -> [Tree]
setRowVisibility row = reverse $ snd $ foldl' setTreeVisibility (-1, []) row

setTreeVisibility :: (Int, [Tree]) -> Tree -> (Int, [Tree])
setTreeVisibility (highest, prev) (Tree h v)
  | h > highest = (h, Tree h True : prev)
  | otherwise   = (highest, Tree h v : prev) -- carries previous visibility

visAllDirection :: Forest -> Forest
visAllDirection f = iterate (rotate . setForestVisibility) f !! 4
  where rotate = fmap reverse . transpose

-- For part2
-- It's better ditch the Tree structure
viewDistance :: Int -> [Int] -> Int
viewDistance h treeh = length (takeWhile' (< h) treeh)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs) | f x       = x : takeWhile' f xs
                      | otherwise = [x]

scenicScore :: [Int] -> Int -> Int
scenicScore row ind = viewDistance h left * viewDistance h right
 where
  (l, r) = splitAt ind row
  left   = reverse l
  right  = drop 1 r
  h      = row !! ind

rowScenicScore :: [Int] -> [Int]
rowScenicScore row = scenicScore row <$> [0 .. (length row - 1)]

part1 :: Forest -> Int
part1 f = length $ concatMap (filter isVisible) (visAllDirection f)

part2 :: Forest -> Int
part2 f = maximum $ concat score
 where
  hForest      = fmap (fmap height) f
  hForestT     = transpose hForest
  vScenicScore = rowScenicScore <$> hForest
  hScenicScore = transpose $ rowScenicScore <$> hForestT
  score        = zipWith (zipWith (*)) vScenicScore hScenicScore



parse :: String -> Forest
parse input = fmap (fmap treeP) (lines input)

main :: IO ()
main = do
  input <- readFile "./../input/Day8.txt"
  let forest = parse input
  print $ part1 forest
  print $ part2 forest
