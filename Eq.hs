{- 
class Eq a where 
        (==) :: a -> a -> Bool
        (\=) :: a -> a -> Bool
type classes

    Equality: x == x = True
    Symmetry: x == y = y == x
    Transitivity: if x == y = True && y == z = True, then x == z = True 
    Extensitivity: if x == y = True and f is a function that also returns a Eq type, f x == f y = True.
    Negation:  x/= y = not (x == y)
-}



module Eq where 

data Pos1 = Pos1 Integer Integer
    deriving (Show, Eq)
instance Pos1 Eq where
    Pos1 x y == Pos1 xx yy = x == x && y == yy

-- not Eqs:
{-
data Pos2 = Pos2 Integer Integer
    deriving (Show)
instance Pos2 Eq where
    Pos2 x y == Pos2 xx yy = x == x || y == yy


data Pos3  = Pos3 Integer Integer
    deriving (Show)
instance Pos3 Eq where
    Pos3 _ _ == Pos3 _ _ = True


data Pos4 = Pos4 Integer Integer
    deriving (Show)
instance Pos4 Eq where
    Pos4 _ _ == Pos4 _ _ = False

-}
data Pos5 = Pos5 (Bool -> Integer)
    deriving (Show, Eq)
instance Pos5 Eq where
    Pos5 f == Pos5 g = f False == g False && f True == g True


