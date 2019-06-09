{-
    Generate random english sentence with correct grammar.
-}
module PAI.Chapter2.Example3 where

import Control.Monad.Random

-- | Typeclass for structure which can generate text
class  TextGen a where
    generate :: (MonadRandom m) => a -> m String
    
    
newtype Grammar = Grammar [Sentence]
data Sentence = Sentence NounPhrase VerbPhrase
data NounPhrase = NounPhrase Article Noun -- ManyAdj Noun
data VerbPhrase = VerbPhrase Verb NounPhrase
newtype Verb = Verb [String]
newtype Noun = Noun [String]
newtype Article = Article [String]
newtype Adjective = Adj [String]
newtype ManyAdj = ManyAdj [String]


instance TextGen Sentence where  
    generate (Sentence nps vps) = do
        xs <- join nps vps
        return $ xs ++ "."

instance TextGen NounPhrase where  
    generate (NounPhrase first second) = join first second

instance TextGen VerbPhrase where  
    generate (VerbPhrase first second) = join first second

instance TextGen Verb where  
    generate (Verb xs) = oneOf xs
        
instance TextGen Noun where  
    generate (Noun xs) = oneOf xs
        
instance TextGen Article where  
    generate (Article xs) = oneOf xs
        
instance TextGen Adjective where  
    generate (Adj xs) = oneOf xs
        
instance TextGen ManyAdj where  
    generate (ManyAdj xs) = manyOf xs
        

-- | Generate and join 2 text
join :: (MonadRandom m, TextGen a, TextGen b) => a -> b -> m String
join a b = do
        x <- generate a
        y <- generate b
        return $ unwords [x, y]
               
-- | Select one element from the list               
oneOf ::  (MonadRandom m) => [a] -> m a
oneOf = fromList . fmap (flip (,) 1)        

-- | Select 0 or more elements from the list without repetition
manyOf ::  (MonadRandom m) => [String] -> m String
manyOf xs = do 
    count <- getRandomR (0,length xs - 1)
    let ys = take count xs
    return $ unwords ys

-- ----------------------------------------------------------------------------    
-- Generate simple grammar for testing purposes
-- ----------------------------------------------------------------------------    

simpleGrammar :: Sentence
simpleGrammar = Sentence nounPhrase verbPhrase

nounPhrase :: NounPhrase                    
nounPhrase = NounPhrase article noun

verbPhrase :: VerbPhrase
verbPhrase = VerbPhrase verb nounPhrase

article :: Article
article = Article ["the", "a"]

noun :: Noun
noun = Noun ["man", "ball", "woman", "table"]

verb :: Verb
verb = Verb ["hit", "took", "saw", "liked"]

adjective :: Adjective
adjective = Adj ["big", "little", "green", "blue"] 
