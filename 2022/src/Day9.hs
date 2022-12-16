{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import           Control.Applicative
import           Data.Attoparsec.Text    hiding ( D
                                                , take
                                                )
import           Data.Foldable                  ( minimumBy )
import           Data.Function                  ( on )
import           Data.List                      ( scanl' )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO

data Move = U | D | R | L deriving Show
type Pos = (Int, Int)

expand :: [(Move, Int)] -> [Move]
expand = concatMap (\(m, n) -> replicate n m)

pathP :: Parser [(Move, Int)]
pathP = moveP `sepBy` endOfLine

moveP :: Parser (Move, Int)
moveP = upP <|> downP <|> rightP <|> leftP

upP, downP, rightP, leftP :: Parser (Move, Int)
upP = (U, ) <$> ("U " *> decimal)
downP = (D, ) <$> ("D " *> decimal)
rightP = (R, ) <$> ("R " *> decimal)
leftP = (L, ) <$> ("L " *> decimal)

parseMove :: Text -> [(Move, Int)]
parseMove input = case parseOnly pathP input of
  Left  _err -> []
  Right path -> path

moveHead :: Pos -> Move -> Pos
moveHead (x, y) U = (x, y + 1)
moveHead (x, y) D = (x, y - 1)
moveHead (x, y) L = (x - 1, y)
moveHead (x, y) R = (x + 1, y)

neighbours :: Pos -> S.Set Pos
neighbours (x, y) =
  S.fromList [ (x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1] ]

closestOf :: Pos -> S.Set Pos -> Pos
closestOf target cand = minimumBy (compare `on` distance target) cand
  where distance (x, y) (x', y') = (x - x') ^ 2 + (y - y') ^ 2

moveTail :: Pos -> Pos -> Pos
moveTail target tail | tail `elem` targetNeighbour = tail
                     | otherwise                   = closestOf target cand
 where
  targetNeighbour = neighbours target
  tailNeighbour   = neighbours tail
  cand            = S.intersection targetNeighbour tailNeighbour

step :: (Pos, [Pos]) -> Move -> (Pos, [Pos])
step (head, tails) m = (newHead, newTails)
 where
  newHead  = moveHead head m
  newTails = tail $ scanl' moveTail newHead tails

countPos :: [(Pos, [Pos])] -> Int
countPos = length . S.fromList . fmap (last . snd)

solution :: (Pos, [Pos]) -> [Move] -> Int
solution start = countPos . scanl' step start

part1 :: [Move] -> Int
part1 = solution ((0, 0), [(0, 0)])

part2 :: [Move] -> Int
part2 = solution ((0, 0), (replicate 9 (0, 0)))

main :: IO ()
main = do
  input <- TIO.readFile "./../input/Day9.txt"
  let moves = expand $ parseMove input
  print $ part1 moves
  print $ part2 moves
