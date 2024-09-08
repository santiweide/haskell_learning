-- ch8 Declaring types and classes


-- type is used to give another name to the existing classes
type Pos = (Int, Int)

type Trans = Pos -> Pos

type Tree = (Int, [Tree])

-- type could be declared with more than one param
type KVList k v = [(k,v)]

find :: Eq k => k -> KVList k v -> v
find k t = head [v | (kk,v) <-t, k == kk]

-- data
-- a completely new type is declared by data
-- could be built with multiple constructors
data MyBool = True | False
data Move = North | South | West | East
move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)


moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (x,xs) p = moves xs (move x p)


rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West

-- data coule be defined with multiple arguments
data Shape = Circile Float | Rect Float Float
square :: Float -> Shape
square n = Rect n  n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Maybe = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
savediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- so useful!

-- newrtype
-- a complete new type is declared by newtype
-- single constructor with a single arguement








