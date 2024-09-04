-- insert
-- requirement: insert x into list , with all elements in list smaller than x
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:xs) = if x < h then x:h:xs else h:(insert x xs)

-- insert sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert x (isort xs)

-- quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
        where
            smaller = [a | a <- xs, a <= x]
            larger  = [b | b <- xs, b > x]

-- zip
zip2 :: [a] -> [b] -> [(a,b)]
zip2 _ [] = []
zip2 [] _ = []
zip2 (x:xs) (y:ys) = [(x,y)] ++ zip2 xs ys

-- drop
drop2 :: Int -> [a] -> [a]
drop2 n [] = []
-- drop2 0 xs = xs
drop2 n xs = case n<=0 of 
    True -> xs
    False -> drop2 (n-1) (tail xs)
-- remember drop2 (-1) [1], there should go with a parenthesise

-- fac
fac :: Int -> Int
fac n = case n<=1 of
    True -> 1
    False -> n * fac (n-1)

-- reverse
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = (reverse2 xs) ++ [x]

-- fib

-- even
-- odd
-- evens
-- odds
-- init

