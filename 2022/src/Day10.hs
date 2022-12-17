{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import           Control.Applicative
import           Data.Attoparsec.Text    hiding ( D
                                                , take
                                                )
import           Data.List.Split         hiding ( sepBy )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO

data Inst = NOOP | ADDX Int deriving Show
type Signal = (Int, Int)

progP :: Parser [Inst]
progP = instP `sepBy` endOfLine

instP :: Parser Inst
instP = noopP <|> addxP

noopP, addxP :: Parser Inst
noopP = NOOP <$ "noop"
addxP = ADDX <$> ("addx " *> signed decimal)

parseProg :: Text -> [Inst]
parseProg input = case parseOnly progP input of
  Left  _err -> []
  Right path -> path

preprocess :: [Inst] -> [Inst]
preprocess = concatMap expand
 where
  expand NOOP     = [NOOP]
  expand (ADDX n) = [NOOP, ADDX n]

process :: Signal -> Inst -> Signal
process (c, s) NOOP     = (c + 1, s)
process (c, s) (ADDX n) = (c + 1, s + n)

scanSignal :: [Inst] -> [Signal]
scanSignal = scanl process (1, 1)

strengthAt :: Int -> [Signal] -> Int
strengthAt instCycle signals = instCycle * snd (signals !! (instCycle - 1))

instCycles :: [Int]
instCycles = [20, 60, 100, 140, 180, 220]

part1 :: [Inst] -> Int
part1 inst = sum $ map (\c -> strengthAt c s) instCycles
  where s = scanSignal $ preprocess inst

isLit :: Signal -> Char
isLit (instCycle, sprite) = if (sprite - pos <= 0) && (pos - sprite <= 2)
  then '#'
  else '.'
  where pos = instCycle `mod` 40

part2 :: [Inst] -> String
part2 inst = unlines (chunksOf 40 (map isLit (scanSignal $ preprocess inst)))

main :: IO ()
main = do
  input <- TIO.readFile "./input/Day10.txt"
  let instruments = parseProg input
  print $ part1 instruments
  putStr $ part2 instruments
