module Maybe where

type Age = Integer

votingAge :: Age -> Bool
votingAge age = age >= 18

-- define Maybe par & ret functions with 1,2,3 params

canVote :: Maybe Age -> Maybe Bool
canVote Nothing = Nothing
canVote (Just a) = Just (votingAge a)

sameAge :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge Nothing _  = Nothing
sameAge _ Nothing = Nothing
sameAge (Just a) (Just b) = Just (a == b)

ordered :: Age -> Age -> Age -> Bool
ordered a b c = (a <= b && b <= c)

isTriple :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple Nothing _ _ = Nothing
isTriple _ Nothing _ = Nothing
isTriple _ _ Nothing = Nothing
isTriple (Just age1) (Just age2) (Just age3) =
    Just (ordered age1 age2 age3)


--maybeMap :: 
--maybeMap


-- now lets unify canvote, same age and triple with Maybe map
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

canVote' :: Maybe Age -> Maybe Bool
canVote' x = maybeMap votingAge x


maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f Nothing _ = Nothing
maybeMap2 f _ Nothing = Nothing
maybeMap2 f (Just x) (Just y) = Just (f x y)

sameAge' :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge' x y = maybeMap2 (==) x y


maybeMap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
maybeMap3 f Nothing _ _ = Nothing
maybeMap3 f _ Nothing _ = Nothing
maybeMap3 f _ _ Nothing = Nothing
maybeMap3 f (Just x) (Just y) (Just z) = Just (f x y z)


isTriple' :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple' x y z = maybeMap3 ordered x y z



-- there is also apply style, one for many arguments
-- the apply could be a pipelined operator because the input, func, output type are all Maybe
apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply Nothing _ = Nothing
apply _ Nothing = Nothing
apply (Just f) (Just a) = Just (f a)


canVote'' :: Maybe Age -> Maybe Bool
canVote'' x = (Just votingAge) `apply` x

sameAge'' :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge'' x y = (Just (==)) `apply` x `apply` y

isTriple'' :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple'' x y z = (Just ordered) `apply` x `apply` y `apply` z

-- instances
{-
instance Functor Maybe where
    fmap = maybeMap

    fmap f x = Just f `apply` x

instance Applicative Maybe where
    pure = Just
    (<*>) = apply
-}

type Name = String
people :: Name -> Maybe Age
people "Homer" = Just 38
people "Bart" = Just 10
people "Lisa" = Just 8
people _ = Nothing


-- Since there Maybe a noname, the input is Maybe Name.
peopleCanVote :: Maybe Name -> Maybe Bool
propleCanVote Nothing _ = Nothing
propleCanVote Just name = canVote (people name) 

-- what if two functions have the save input type and the same output type, and A is calling B?
-- here comes Monads!
-- Here if peopleCanVote calls the canVote, to write it simple, we could have:
-- peopleCanVote' :: MaybeName -> MaybeBool
-- peopleCanVote' name = foo name (\name -> canVote (prople name))
-- where
-- foo :: Maybe a -> (a -> Maybe b) -> Maybe b




