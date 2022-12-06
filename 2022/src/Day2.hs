module Day2 where

data Move = Rock | Paper | Scissors deriving (Show, Eq, Enum)

data Outcome = Lose | Draw | Win deriving (Show, Eq, Enum)

parseMove :: Char -> Move
parseMove m | m `elem` "AX" = Rock
            | m `elem` "BY" = Paper
            | otherwise = Scissors

parseOutcome :: Char -> Outcome
parseOutcome o | o =='X' = Lose
              | o =='Y' = Draw
              | otherwise = Win

parsePlay :: Move -> Move -> (Move, Move)
parsePlay a b = (a, b)

getOutcome :: (Move, Move) ->Outcome
getOutcome m | m ==(Rock, Paper) = Win
               | m ==(Paper, Scissors) = Win
               | m ==(Scissors, Rock) = Win
               | m ==(Rock, Scissors) = Lose
               | m ==(Scissors, Paper) = Lose
               | m ==(Paper, Rock) = Lose
               | otherwise = Draw

parse :: String -> (Move, Move)
parse [x, _, y] = (parseMove x, parseMove y)

parse2 :: String -> (Move, Move)
parse2 [x, _, y] = (mx, getMove (mx, parseOutcome y))
  where mx = parseMove x

getMove :: (Move, Outcome) -> Move
getMove (m, o) | o == Win = winMove m
               | o == Draw = m
               | otherwise = loseMove m

winMove :: Move -> Move
winMove m | m == Scissors = Rock
          | otherwise = succ m

loseMove :: Move -> Move
loseMove m | m == Rock = Scissors
           | otherwise = pred m

moveScore :: Move -> Int
moveScore m = 1 + fromEnum m

outcomeScore :: Outcome ->Int
outcomeScore o = 3 * fromEnum o

countScore :: (Move, Move) -> Int
countScore p = os +ms
  where
    os = outcomeScore (getOutcome p)
    ms = moveScore (snd p)

input :: IO String
input = readFile "./../input/Day2.txt"

main :: IO ()
main = do
  ml <- lines <$> input
  print $ sum (map (countScore . parse) ml)
  print $ sum (map (countScore . parse2) ml)
