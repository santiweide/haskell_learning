-- casesarEncoder 
-- casear encoder is a encoder that reads a string and get a string which each letter is moved forwards 3 steps.


import Data.Char

isSmall :: Char -> Bool
isSmall ch = 'a' <= ch && ch <= 'z'

isCap :: Char -> Bool
isCap ch = 'A' <= ch && ch <= 'B'

ch2int :: Int->Char -> Int
ch2int mode ch = case mode of
        0 -> ord ch - ord 'a' 
        1 -> ord ch - ord 'A'

int2ch :: Int->Int -> Char
int2ch mode x = case mode of
        0 -> chr (ord 'a' + (x `mod` 26))
        1 -> chr (ord 'A' + (x `mod` 26))

getCasearCh :: Int -> Char -> Char
getCasearCh step ch = case (isSmall ch, isCap ch) of
                (True, False) ->  (int2ch 0 (ch2int 0 ch + step))
                (False, True) ->  (int2ch 1 (ch2int 1 ch + step))           
                (False, False) -> ch

casesarEncoder :: Int -> String -> String
casesarEncoder step str = [getCasearCh step ch | ch <- str]


-- freq
-- freq is a function that get the occurancy by % for each letter in a string
-- Capital letter and Smaller letter are considered the same letter

getPercent :: Int -> Int -> Double
getPercent a b = 100 * (fromIntegral a / fromIntegral b)

getLower :: Char -> Char
getLower ch = case isCap ch of
            False -> ch
            True -> chr (ord ch - ord 'a' + ord 'A')

getAllLower :: String -> String
getAllLower str = [getLower ch | ch <- str]

count :: Char -> String -> Int
count ch str = length [s | s<-str, s == ch]

freqs :: String -> [Double]
freqs str = [getPercent (count ch str) (length n) | ch <- ['a'..'z']  ]
                where n = getAllLower str


-- use chi-square statics to proove the which factor in casear encoder is at the most probability
-- chi-square:
-- comparing a list of observed frequency
--  with a list of expected frequency
--  sum i=0..n-1, (oes_i-es_i)**2 / es_i

sumOfList :: [Double] -> Double
sumOfList xs = case xs of
            [] -> 0.0
            (x:xxs) -> x + sumOfList xxs



chisqr :: [Double] -> [Double] -> Double
chisqr oes es = sumOfList [ (oes !! i - es !! i) ** 2 / es !! i | i <- [0..n-1]]
                where n = length es

-- extra:
-- what about 
-- using the library function zip and a list comprehension 
-- for a cis-square function implement?

chisqr_v2 :: [Double] -> [Double] -> Double
chisqr_v2 os es = sum [ (o-e)**2/e | (o,e) <-zip os es]



-- rotate
-- make rotate for n step in s String
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


-- freqs: transfer the shift of alphabet into the shift of list
--      (clever! but only works in statics
table :: [Double]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

table'=freqs "kdvnhoo lv ixq"
cistable=[chisqr (rotate n table') table | n <- [0..25]]

getMin :: Ord a => [a] -> a
getMin [] = error "empty list"
getMin [x] = x
getMin (x:xs) = min x (getMin xs)

getPos :: Ord a => a -> [a] -> [Int]
getPos x xs = [i | i <- [0..length xs - 1], xs !! i == x]

getMinPos :: Ord a => [a] -> [Int]
getMinPos xs = getPos (getMin xs) xs


-- how to crack the casear encoded string with unkonwn shifting n
-- we use crack here to say it is not a 100% right decoding method
-- Attention! where should have strict table align
-- Note that when we use head to select a min pos, there is also some uncertainty
crack :: String -> String
crack xs = casesarEncoder (-step) xs 
        where 
            step     = head (getMinPos chitable)
            chitable = [chisqr (rotate n table') table | n <- [0..25]]
            table'   = freqs xs



-- Exercises
-- 1. Using a list comprehension, give an expression that calculates the sum of 
--  1^2, 2^2, .., 100^2 of the first one hundred integer squares.

calcs :: Int -> Int
-- this is not a list comprehension: calcs n = if n <= 1 then 1 else n * n + calcs (n-1)
calcs n = sum [i*i | i<-[1..n]]


-- grid
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x<-[0..n], y<-[0..m] ]

-- square
square :: Int -> [(Int, Int)]
square n = [ p | p<-(grid n n), fst p /= snd p]

-- replicate
replicate2 :: Int -> a -> [a]
replicate2 0 x = []
replicate2 n x = [x] ++ replicate2 (n-1) x
-- list comprehension version:
replicate3 :: Int -> a -> [a]
replicate3 n x = [x | i<-[1..n]]

-- pyths
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x+y*y==z*z]

-- factors
factors :: Int -> [Int]
factors n = [x| x<-[1..n-1], n `mod` x == 0]

-- perfect
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x)) == x ]


-- re-express
p=[(x,y) | x <- [1,2], y <- [3,4]]
q=concat [ [(x,y) | y<- [3,4]] | x<-[1,2] ]

-- find
-- find v with k in (k,v)
find :: Eq a => a->[(a,b)] -> [b]
find a abs = [ y | (x,y) <- abs, x == a]

-- position
-- Redefine the function positions using the function find
positions :: Eq a => a -> [a] -> [Int]
positions t xs = [y | (x,y) <- zip xs [0..n-1], x == t]
                where n = length xs


-- scalarproduct
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys]






