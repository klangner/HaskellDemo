{-
    Generate random english sentence with correct grammar.
-}
module PAI.Chapter2.Example2 where

import Control.Monad.Random


data Grammar = Sentence Grammar Grammar
             | Phrase Grammar Grammar
             | OneOf [String]
             | ManyOf [String]
             
generate :: (MonadRandom m) => Grammar -> m String
generate (Sentence first second) = do
        text <- generate (Phrase first second)
        return $ text ++ "."
generate (Phrase first second) = do
        x <- generate first
        y <- generate second
        return $ unwords [x, y]
generate (OneOf xs) = oneOf xs
        where oneOf = fromList . fmap (flip (,) 1)
generate (ManyOf xs) = do
        count <- getRandomR (0,length xs - 1)
        let ys = take count xs
        return $ unwords ys
    
    
-- ----------------------------------------------------------------------------    
-- Generate simple grammar for testing purposes
-- ----------------------------------------------------------------------------    

simpleGrammar :: Grammar
simpleGrammar = Sentence nounPhrase verbPhrase

nounPhrase :: Grammar                    
nounPhrase = Phrase article noun

verbPhrase :: Grammar
verbPhrase = Phrase verb nounPhrase

article :: Grammar
article = OneOf ["the", "a"]

noun :: Grammar
noun = OneOf ["man", "ball", "woman", "table"]

verb :: Grammar
verb = OneOf ["hit", "took", "saw", "liked"]


-- ----------------------------------------------------------------------------    
-- More complex grammar
-- ----------------------------------------------------------------------------    

grammar2 :: Grammar
grammar2 = Sentence nounPhrase2 verbPhrase2

nounPhrase2 :: Grammar                    
nounPhrase2 = Phrase article adjNoun

verbPhrase2 :: Grammar
verbPhrase2 = Phrase verb nounPhrase2

adjNoun :: Grammar                
adjNoun = Phrase adjective noun

adjective :: Grammar
adjective = ManyOf ["big", "little", "blue", "green"]

