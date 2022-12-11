{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Control.Applicative
import           Data.Attoparsec.Text    hiding ( take )
import           Data.Char
import           Data.List                      ( foldl'
                                                , sort
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Data.Tree
import           Data.Tree.Zipper               ( Full
                                                , TreePos
                                                , children
                                                , firstChild
                                                , fromTree
                                                , insert
                                                , label
                                                , modifyLabel
                                                , next
                                                , parent
                                                , root
                                                , toTree
                                                )

data Cmd = CD String
  | LS
  | CDIR String
  | CFILE Int String
  deriving Show

data Directory = Dir String (M.Map String Int)
  deriving (Show, Eq)

data DirSize = DSize String Int
  deriving (Show, Eq)

type DTree = Tree Directory
type ZTree = TreePos Full Directory
type STree = Tree DirSize

mkTree :: [Cmd] -> DTree -> DTree
mkTree trace tree = toTree $ root $ makeTree trace $ fromTree tree

makeTree :: [Cmd] -> ZTree -> ZTree
makeTree trace tree = foldl' processCommand tree trace

convertSizeTree :: DTree -> STree
convertSizeTree (Node { rootLabel = (Dir name files), subForest = sf }) =
  (Node { rootLabel = (DSize name size), subForest = sizedTree })
 where
  size      = M.foldl (+) 0 $ M.map fromIntegral files
  sizedTree = convertSizeTree <$> sf

sumSizeTree :: STree -> STree
sumSizeTree (Node { rootLabel = (DSize name size), subForest = sf }) =
  (Node { rootLabel = (DSize name (size + subSizes))
        , subForest = subForestSize
        }
  )
 where
  subForestSize = sumSizeTree <$> sf
  subSizes      = sum $ (getSize . rootLabel) <$> subForestSize

getSize :: DirSize -> Int
getSize (DSize _ s) = s

processCommand :: ZTree -> Cmd -> ZTree
processCommand tree (CD dir) | dir == "/"  = root tree
                             | dir == ".." = fromJust $ parent tree
                             | otherwise   = fromJust $ getDir dir tree
processCommand tree LS = tree
processCommand tree (CFILE size name) =
  modifyLabel (\(Dir n fs) -> Dir n (M.insert name size fs)) tree
processCommand tree (CDIR name) = if isJust $ getDir name tree
  then tree
  else
    fromJust
    $ parent
    $ insert (Node { rootLabel = Dir name M.empty, subForest = [] })
    $ children tree

getDir :: String -> ZTree -> Maybe ZTree
getDir name tree = findDir name (firstChild tree)

findDir :: String -> Maybe ZTree -> Maybe ZTree
findDir _ Nothing = Nothing
findDir name (Just tree) | name == labelName = Just tree
                         | otherwise         = findDir name (next tree)
  where (Dir labelName _) = label tree

-- Parsers
traceP :: Parser [Cmd]
lineP :: Parser Cmd
cdP :: Parser Cmd
lsP :: Parser Cmd
fileP :: Parser Cmd
dirP :: Parser Cmd
letterP :: Parser Char
nameP :: Parser String

traceP = lineP `sepBy` endOfLine

lineP = cdP <|> lsP <|> fileP <|> dirP
cdP = CD <$> ("$ cd " *> nameP)
lsP = LS <$ "$ ls"
fileP = CFILE <$> (decimal <* " ") <*> nameP
dirP = CDIR <$> ("dir " *> nameP)

nameP = many1 letterP
letterP = satisfy (not . isSpace)

parseTrace :: Text -> [Cmd]
parseTrace input = case parseOnly traceP input of
  Left  _err     -> []
  Right commands -> commands

-- solving the problem
emptyTree :: DTree
emptyTree = Node { rootLabel = (Dir "/" M.empty), subForest = [] }

part1 :: STree -> Int
part1 tree = sum (getSmallSize <$> (flatten tree))

getSmallSize :: DirSize -> Int
getSmallSize (DSize _ s) = if s < 100000 then s else 0

part2 :: STree -> Int
part2 tree = head $ sort candidates
 where
  used       = getSize $ rootLabel tree
  target     = used - 40000000
  nodes      = getSize <$> flatten tree
  candidates = filter (>= target) nodes

main :: IO ()
main = do
  l <- TIO.readFile "./../input/Day7.txt"
  let trace    = parseTrace l
  let tree     = mkTree trace emptyTree
  let sizeTree = sumSizeTree $ convertSizeTree tree
  print $ part1 sizeTree
  print $ part2 sizeTree
