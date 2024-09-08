-- functors, applicatives and Monads
-- all three of them introduced are examples of the idea of abstracting out a common programming pattern as a definition
-- that maybe what is call "design pattern"
--
--Functors
--functors abstracts the idead of mapping a function over each element of a sttructure.
--limitation: only one argument is okay
--laws:
--  fmap id = id
--  fmap (g.h) = fmap g . fmap h

instance Functor f where
    --fmap :: (a->b) -> f a -> f b
    fmap = map
-- above : f is a instance of Functor, and the way it behaves like a functor is
--  when we call fmap, it behaves like xxx.


--Applicatives
--applicatives allow the functions with any number of arguements to be mapped, rather than being restricted to functions with a single argument.

class Functor f => Applicative f where 
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- that is, an applicatived data or newtype  must be a Functor
-- pure and <*> are just defined for the number of arguments of the function could be extended







-- maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- maybeBind Nothing _ -> Nothing
-- maybeBind (Just x) f -> f x
-- 
-- eitherBind :: Either e a -> (a -> Either e b) -> Either e b
-- eitherBind (Left e) _ -> (Left e)
-- eitherBind (Right x) f = f x
-- 
-- -- the type list is a chain, so we could pass types thriugh monad
-- listBind :: [a] -> (a -> [b]) -> [b]
-- listBind [] _ = []
-- listBind (x:xs) f = (f x) ++ (listBind xs f)
-- 
-- class Applicative m => Monda m where
--     (>>=) :: m a -> (a -> m b) -> (m b)
-- 
-- instance Monda Maybe where
--     Nothing >>= _ = Nothing
--     Just x >>= f = f x
-- 
-- instance Monda Maybe where
--     (>>=) = maybeBind
-- 
-- -- pure the x
-- liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f x = x >>= \x' -> pure (f x')
-- 
-- 
-- ap :: m (a -> b) -> m a -> m b
-- ap f x = f >>= \f' -> 
--         x >>= \x' -> 
--             pure (f' x')
-- 
-- 
-- do x <- foo
--     y <- bar
--     baz x y



newtype Reader env a = Reader (env -> a)

instance Funtor (Reader env) where
    fmap = liftM

instance Applicative (Reader env) where
    (<*>) = ap
    pure x = undefined

instance Monad (Reader env) where
    m >>= f = undefined






