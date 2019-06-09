{-
    Generate random english sentence with correct grammar.
-}
module PAI.Chapter2.Example1 where

import Control.Monad.Random


-- | noun phrase + verb phrase
sentence :: (MonadRandom m) => m String
sentence = do
    np <- nounPhrase
    vp <- verbPhrase
    return  $ np ++ " " ++ vp ++ "."


-- | article + noun 
nounPhrase :: (MonadRandom m) => m String
nounPhrase = do
    a <- article
    n <- noun
    return $ a ++ " " ++ n


-- | verb + noun phrase
verbPhrase :: (MonadRandom m) => m String
verbPhrase = do
    v <- verb
    n <- nounPhrase
    return $ v  ++ " " ++ n 


-- | Select english article
article :: (MonadRandom m) => m String
article = oneOf ["the", "a"]


-- | Select english noun
noun :: (MonadRandom m) => m String
noun = oneOf ["man", "ball", "woman", "table"]


-- | Select english verb 
verb :: (MonadRandom m) => m String
verb = oneOf ["hit", "took", "saw", "liked"]


-- | Sample a value from a uniform distribution of a list of elements.
oneOf :: (MonadRandom m) => [a] -> m a
oneOf = fromList . fmap (flip (,) 1)