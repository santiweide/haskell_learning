-- Chapter 7 Higher-order functions
-- function can also be a type, written as (t->t)

import Data.Char
type Bit = Int

-- twice
twice :: (t -> t) -> t -> t
twice f x = f (f x)

myadd :: Num a => a -> a
myadd n = n + n
-- map
-- map is to apply a function to all elements in a list
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

-- filter
-- filter is to apply a Bool function to all element in a list 
-- and get the qualified elements in another list
-- where the final list gets True with the Bool function
filter2 :: (a -> Bool) -> [a]  -> [a]
filter2 f xs = [x | x <- xs, f x == True]

-- sumsqreven
sumsqreven :: [Bit] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))


-- Binary string transmitter
-- bin2int tries to make a list of 0/1 into a Integer decimal
bin2int :: [Int] -> Int
bin2int xs = sum [w*b| (w,b) <- zip weight xs]
        where weight = iterate (*2) 1

bin2int2 :: [Bit] -> Int
bin2int2 = foldr (\x y -> x + 2*y) 0

-- int2bin
int2bin :: Int -> [Bit]

int2bin x = case x > 0 of
        True -> x `mod` 2 : int2bin (x `div` 2)
        False -> []

-- make8
-- makes a fixed length list of Bits with an existing list
make8 :: [Bit] -> [Bit]
make8 xs = xs ++ take 8 (repeat 0)

-- encodes
-- define a function that encodes a string of characters as a list of bits 
-- by converting each character into a Unicode number,
-- converting each such number into an eight-bit binary number, 
-- and concatenating each of these numbers together to produce a list of bits. 
-- Using the higher-order functions map and composition
encodes1 :: [Char] -> [Bit]
encodes1 = concat . map (make8 . int2bin . ord)

encodes :: [Char] -> [Bit]
encodes chs = concat (map make8 [int2bin (ord ch) | ch <- chs])


-- Question:
-- will we see higher order function as a Syntactic sugar 
-- or there is something that only higher order functions could do?


-- chop8 make a list into [[$8bits]]
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- decode
-- decode a Bit list to a string
decode1 :: [Bit] -> String
decode1 = map (chr . bin2int) . chop8
decode bits = map (chr . bin2int) (chop8 bits)


-- (.) :: (b -> c) -> (a -> b) -> (a -> c) 
-- f . g = \x -> f (g x)


-- Exercises
-- [f x | x <- xs, p x]

add1 :: Num a => a->a
add1 n = n+1

foo :: (a->a) -> (a-> Bool) -> [a] -> [a]
foo f p xs = [f x | x <- xs, p x]

foo2 :: (a->a) -> (a-> Bool) -> [a] -> [a]            
foo2 f p xs = map f (filter p xs)


all2 p = and . map p

-- b. Decide if any element of a list satisfies a predicate:
any2 p = or . map p


takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p [] = []
takeWhile2 p (x:xs) = case p x of 
        True -> [x] ++ (takeWhile2 p xs)
        False -> takeWhile2 p xs

dropWhile2 :: (a->Bool) -> [a] -> [a]
dropWhile2 p [] = []
dropWhile2 p (x:xs) = case p x of 
        True -> dropWhile2 p xs
        False -> [x] ++ (dropWhile2 p xs)


-- Redefine the functions map f and filter p using foldr.

