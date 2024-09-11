-- def a instance of functor class for the following type of binary tree that have data in their nodes

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)


instance Applicative Tree where
    -- pure ::  a- > Tree a
    -- (<*>) :: Tree (a-> b) -> Tree a -> Tree b

instance fmap Tree where 
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap 

