module Parsers where

{--
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    (<*>) :: f (a -> b) -> (f a -> f b)
    pure :: a -> f a
-}

-- Some examples
data Option a = None | Some a
    deriving Show

instance Functor Option where
    fmap f (Some a) = Some (f a)
    fmap f None = None


type Reader r a = r -> a

instance Functor ((->) r) where
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = \r -> f . g


-- Parser
data ParserResult a = Failed String | Ok a String

instance Functor ParserResult where
    fmap f (Ok a xs) = Ok (f a) xs
    fmap _ r = r


type Parser a = String -> ParserResult a