-- foo.hs
f :: Int -> Int
f x = x * 2


get_pos :: Int->String->Char
get_pos = undefined

fib :: Int -> Int
fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)


-- data MyBool we name new datatype with "data"
data MyBool = Sure | Nope
    deriving (Show)


val :: MyBool -> Int
val q = if q == Sure then 0 else 1 -- errors for Eq
val q = case q of
    Sure -> 0
    Nope -> 1

-- else
val Sure = 0
val Nope = 1


-- for all warnings
:set -Wall 

data List = 
    Nil 
    | Cons Int List
    deriving(Show)

data String =
    Nil
    | Cons Char List
    deriving(Show)

length :: List -> Int
length xs = if null xs then 0 else 1 + length tail xs

listLength Nil = 0
listLength (Cons _ tail_eles) = 1 + listlength tail_eles

data AString = 
    Nil
    | Cons a (List a)
    deriving(Show)


