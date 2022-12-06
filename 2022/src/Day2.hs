module Day2 where

data Move = Rock | Paper | Scissors | Unknown deriving (Show, Eq, Enum)

data Outcome = Lose | Draw | Win deriving (Show, Eq, Enum)

parseMove :: Char -> Move
parseMove m | m `elem` "AX" = Rock
            | m `elem` "BY" = Paper
            | m `elem` "CZ" = Scissors
            | otherwise = Unknown

parseOutcome :: (Move, Move) ->Outcome
parseOutcome m | m ==(Rock, Paper) = Win
               | m ==(Paper, Scissors) = Win
               | m ==(Scissors, Rock) = Win
               | m ==(Rock, Scissors) = Lose
               | m ==(Scissors, Paper) = Lose
               | m ==(Paper, Rock) = Lose
               | otherwise = Draw

parsePlay :: Move -> Move -> (Move, Move)
parsePlay a b = (a, b)

parse :: String -> (Move, Move)
parse s = (head ls, head (tail ls))
  where ls = filter  (/= Unknown) (map parseMove s)

moveScore :: Move -> Int
moveScore m = 1 + fromEnum m

outcomeScore :: Outcome ->Int
outcomeScore o = 3 * fromEnum o

countScore :: (Move, Move) -> Int
countScore p = os +ms
  where
    os = outcomeScore (parseOutcome p)
    ms = moveScore (snd p)

input :: IO String
input = readFile "./../input/Day2.txt"

main :: IO ()
main = do
  ml <- lines <$> input
  print $ sum (map (countScore . parse) ml)
