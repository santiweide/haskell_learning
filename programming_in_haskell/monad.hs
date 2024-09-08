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







