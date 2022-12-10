module ZipperTree where

data Tree a = Leaf a | Bin (Tree a) (Tree a)

data Branch a = LeftNode (Tree a)
              | RightNode (Tree a)

type Zipper a = (Thread a, Tree a)

type Thread a = [Branch a]

get :: Tree a -> a
get (Leaf a) = a
get (Bin _ _) = error "Not a leaf"

isLeaf :: Tree a -> Bool
isLeaf (Leaf a) = True
isLeaf _ = False

leftNode :: Tree a -> Maybe (Tree a)
leftNode (Leaf _) = Nothing
leftNode (Bin l r) = Just l

rightNode :: Tree a -> Maybe (Tree a)
rightNode (Leaf _) = Nothing
rightNode (Bin l r) = Just r

turnRight :: Maybe (Zipper a) -> Maybe (Zipper a)
turnRight Nothing = Nothing
turnRight (Just (_, Leaf _)) = Nothing
turnRight (Just (t, Bin l r)) = Just (LeftNode l : t, r)

turnLeft :: Maybe (Zipper a) -> Maybe (Zipper a)
turnLeft Nothing = Nothing
turnLeft (Just (_, Leaf _)) = Nothing
turnLeft (Just (t, Bin l r)) = Just (RightNode r : t, l)

parent :: Maybe (Zipper a) -> Maybe (Zipper a)
parent Nothing = Nothing
parent (Just ([], _)) = Nothing
parent (Just (LeftNode l : t, r)) = Just (t, Bin l r)
parent (Just (RightNode r : t, l)) = Just (t, Bin l r)

testTree :: Tree Int
testTree = Bin (Bin (Leaf 1) (Leaf 2)) (Bin (Leaf 3) (Leaf 4))

--     .
--    / \
--   .   .
--  / \ / \
--  1 2 3 4

testZipper :: Maybe (Zipper Int)
testZipper = Just ([], testTree)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error "Nothing here"

res = get (snd (fromJust (turnRight (parent (turnLeft (turnRight testZipper)))))) -- should be 4
