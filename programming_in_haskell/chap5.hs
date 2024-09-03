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




