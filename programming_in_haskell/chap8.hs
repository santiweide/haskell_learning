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

-- new type
-- a complete new type is declared by newtype
-- single constructor with a single arguement


newtype Nat = N Int
-- using newtype rather than type means Nat and Int are different types rather than stnonyms
-- using newtype rather than data brings an efficient benefit, 
-- because newtype constructs such as N do not incur any cost when programs are evaluated, 
-- and they are removed once the typing check is complete
-- using newtype is to garuntee the type safe while not influence the performance

-- recursive types

data Nat = Zero | Succ Nat
nat2int :: Nat -> int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)


add Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = 1 + (add m n)

data List = Nul | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons a b)) = 1 + (len b)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
-- we can leave Leaf and Node there without caring about them


t :: Tree Int
t = Node (Node (Leaf 3) 1 (Leaf 4 )) 5
    (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l y r) = flatten l ++ y ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                        | x < y = occurs l
                        | otherwise = occurs r

-- class and instance declaration
--
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)

instance Eq Bool where 
    False == False = True
    True == True = True
    _ == _ = False

class Eq a => Ord a where 
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max :: a -> a -> a
    min x y | x <= y  = x
            | otherwise = y
    max x y | x <= y = y
            | other wise = x

-- Ord in system lib could be extended:
-- this means: Bool is an instance of Order
instance Ord Bool where
    False < True = True
    _ < _ = False


-- Note that only types that are declared using data or newtype could be made into instances of classes


-- derived instances
-- if we what Shape to be a Equality type, Float has to be a Equality type

