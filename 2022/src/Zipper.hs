module Zipper where

data Node a = DeadEnd a
              | Passage a (Node a)
              | Fork a (Node a) (Node a)

get :: Node a -> a
get (DeadEnd a) = a
get (Passage a _) = a
get (Fork a _ _) = a

put :: a -> Node a -> Node a
put x (DeadEnd _) = DeadEnd x
put x (Passage _ p) = Passage x p
put x (Fork _ l r) = Fork x l r

data Branch a = KeepStraightOn a
              | TurnLeft a (Node a)
              | TurnRight a (Node a)

type Thread a = [Branch a]

type Zipper a = (Thread a, Node a)

turnRight :: Maybe (Zipper a) -> Maybe (Zipper a)
turnRight Nothing = Nothing
turnRight (Just (t, Fork x l r)) = Just (TurnRight x l : t, r)
turnRight _ = Nothing

turnLeft :: Maybe (Zipper a) -> Maybe (Zipper a)
turnLeft Nothing = Nothing
turnLeft (Just (t, Fork x l r)) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

keepStraight :: Maybe (Zipper a) -> Maybe (Zipper a)
keepStraight Nothing = Nothing
keepStraight (Just (t, Passage x p)) = Just (KeepStraightOn x : t, p)
keepStraight _ = Nothing

back :: Maybe (Zipper a) -> Maybe (Zipper a)
back Nothing = Nothing
back (Just ([], _)) = Nothing
back (Just (KeepStraightOn x : t, n)) = Just (t, Passage x n)
back (Just (TurnLeft x r : t, l)) = Just (t, Fork x l r)
back (Just (TurnRight x l : t, r)) = Just (t, Fork x l r)

-- retrieve is not needed
-- retrieve :: Thread -> Node a -> a
-- retrieve [] n = get n
-- retrieve (KeepStraightOn : bs) (Passage _ n) = retrieve bs n
-- retrieve (TurnLeft : bs) (Fork _ l r) = retrieve bs l
-- retrieve (TurnRight : bs) (Fork _ l r) = retrieve bs r

update :: Node a -> (a -> a) -> Node a
update (DeadEnd a) f = DeadEnd (f a)
update (Passage a n) f = Passage (f a) n
update (Fork a l r) f = Fork (f a) l r

labyrinth :: Node (Int, Int)
labyrinth = Fork (0, 2)
  (Fork (-2, 0) (DeadEnd (-1, 0)) (DeadEnd (0, -2)))
  (Passage (2, 0)
    (Fork (1,0) (Passage (0, 1) (DeadEnd (0, 0))) (DeadEnd (0, -1))))

start :: Maybe (Zipper (Int, Int))
start = Just ([], labyrinth)

next = turnRight start

getJust :: Maybe a -> a
getJust (Just a) = a
getJust _ = error "Nothing here"


