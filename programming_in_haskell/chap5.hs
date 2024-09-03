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

freq :: String -> [Double]
freq str = [getPercent (count ch str) (length n) | ch <- ['a'..'z']  ]
                where n = getAllLower str

