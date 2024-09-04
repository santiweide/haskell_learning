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
fib :: Int -> Int
fib n = case n <=2 of
    True -> case n <=0 of
        True -> 0
        False -> 1
    False -> fib (n-1) + fib (n-2) 

-- even
even2 :: Int -> Bool
even2 0 = True
even2 n = odd2 (n-1)

-- odd
odd2 0 = False
odd2 n = even2 (n-1)

-- evens
-- get all numbers in a list that on the even place
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = x:(odds xs)

-- odds
odds :: [Int] -> [Int]
odds [] = []
odds (x:xs) = evens xs


-- init
init2 :: [a] -> [a]
init2 [] = []
init2 [_] = []
init2 (x:xs) = x : init2 xs


--- Exercises
-- sumdown
sumdown :: Int -> Int
sumdown n = case n<=0 of
    True -> 0
    False -> n + sumdown (n-1)

-- (^)
(@) :: Double -> Int -> Double
a @ 0 = 1
1 @ n = 1
a @ n = case n < 0 of
    True -> 1 / (a @ (-n) )
    False -> a * (a @ (n-1))

-- euclid
euclid :: Int -> Int -> Int 
euclid a b = case a < b of 
    True -> euclid (b-a) a
    False -> case a == b of
        True -> a
        False -> euclid (a-b) b

-- merge
-- merge two ordered list (increasing)
merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys) = case x < y of 
    True  -> (x : (merge xs (y:ys) ))
    False -> (y : (merge (x:xs) ys ))

-- merge sort
getFront :: Int -> [a] -> [a]
getFront 0 _ = []
getFront n [] = []
getFront n (x:xs) = [x] ++ getFront (n-1) xs

getBack :: Int -> [a] -> [a]
getBack 0 _ = []
getBack n [] = []
getBack n xs = reverse (take n (reverse xs))

msort :: Ord a => [a] -> [a]
msort xs = case length xs of
        0 -> []
        1 -> xs
        2 -> merge [head xs] (tail xs)
        otherwise -> merge (msort l) (msort r)
            where   l = take len_l xs
                    r = getBack len_r xs
                    len_l = (length xs) `div` 2
                    len_r =  (length xs) - len_l



