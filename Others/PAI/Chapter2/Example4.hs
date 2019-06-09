{-
    Generate random english sentence with correct grammar.
-}
module PAI.Chapter2.Example4 where

import Control.Monad.Random

-- | Structure which defines sentence generator
data SentenceGen = Sentence {nounPhraseGen ::SentenceGen, verbPhraseGen::SentenceGen }
                 | NounPhrase {articleGen::SentenceGen, nounGen ::SentenceGen}
                 | VerbPhrase {verbGen::SentenceGen, nounPhraseGen ::SentenceGen}
                 | Noun [String]
                 | Verb [String]
                 | Article [String]
             
             
generate :: (MonadRandom m) => SentenceGen -> m String
generate (Sentence a b) = do
        xs <- join a b
        return $ xs ++ "."
generate (NounPhrase a b) = join a b
generate (VerbPhrase a b) = join a b
generate (Noun xs) = oneOf xs
generate (Verb xs) = oneOf xs
generate (Article xs) = oneOf xs


-- Generate and join 2 text
join :: (MonadRandom m) => SentenceGen -> SentenceGen -> m String
join a b = do
        x <- generate a
        y <- generate b
        return $ unwords [x, y]

-- Select one from list    
oneOf ::    (MonadRandom m) => [a] -> m a
oneOf = fromList . fmap (flip (,) 1)

    
-- ----------------------------------------------------------------------------    
-- Generate simple grammar for testing purposes
-- ----------------------------------------------------------------------------    

simpleGrammar :: SentenceGen
simpleGrammar = Sentence nounPhrase verbPhrase

nounPhrase :: SentenceGen                    
nounPhrase = NounPhrase article noun

verbPhrase :: SentenceGen
verbPhrase = VerbPhrase verb nounPhrase

article :: SentenceGen
article = Article ["the", "a"]

noun :: SentenceGen
noun = Noun ["man", "ball", "woman", "table"]

verb :: SentenceGen
verb = Verb ["hit", "took", "saw", "liked"]

