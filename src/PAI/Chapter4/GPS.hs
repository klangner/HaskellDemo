{- General Problem Solver
-}
module PAI.Chapter4.GPS where

import Data.List


data Operation = Operation { actionName :: String
                           , preconditions :: [State]
                           , addStates :: [State]
                           , removeStates :: [State] } deriving (Show)
                           
type State = String

-- Run simulation
gps :: [State]      -- Current state
    -> [State]      -- Desired state
    -> [Operation]  -- List of allowed operations
    -> Maybe [Operation]  -- Steps taken to get to destination state
gps state dest ops | null diff = Just []
                   | (not . null) actions = case gps state dest' ops of
                                        Just actions2 -> Just (head actions:actions2)
                                        Nothing -> Nothing
              | otherwise = Nothing
              where diff = dest \\ state  -- What is needed
                    actions = findActions diff ops -- Which action can solve one of the needs
                    dest' = updateDest dest (head actions)


-- Find allowed action which changes state
findActions :: [State] -> [Operation] -> [Operation]
findActions dest = filter f
    where f op = (addStates op `intersect` dest) /= [] 
    
    
-- Apply operation and change state
updateDest :: [State] -> Operation -> [State]
updateDest states op = (states `union` preconditions op) \\ addStates op
    

-- ----------------------------------------------------------------------------
-- Demo                           
-- ----------------------------------------------------------------------------

demo1 :: IO ()
demo1 = do
    let ops = gps ["son-at-home", "car-works"] ["son-at-school"] schoolOps
    case ops of
        Just actions -> do 
            putStrLn "Solution found:"
            mapM_ (putStrLn . actionName) actions
        Nothing -> putStrLn "No solutions."
    
    
demo2 :: IO ()
demo2 = do
    let ops = gps ["son-at-home", "car-needs-battery", "have-money", "have-phone-book"] ["son-at-school"] schoolOps
    case ops of
        Just actions -> do 
            putStrLn "Solution found:"
            mapM_ (putStrLn . actionName) (reverse actions)
        Nothing -> putStrLn "No solutions."
    
    
              
-- Same school operations                  
schoolOps :: [Operation]
schoolOps = [ Operation "drive-son-to-school" 
                        ["son-at-home", "car-works"] 
                        ["son-at-school"] ["son-at-home"]
            , Operation "shop-installs-battery" 
                        ["car-needs-battery", "shop-knows-problem", "shop-has-money"]
                        ["car-works"] []
            , Operation "tell-shop-problem"
                        ["in-communication-with-shop"]
                        ["shop-knows-problem"] []
            , Operation "telephone-shop"
                        ["know-phone-number"]
                        ["in-communication-with-shop"] []
            , Operation "look-up-number"
                        ["have-phone-book"]
                        ["know-phone-number"] []
            , Operation "give-shop-money"
                        ["have-money"]
                        ["shop-has-money"] ["have-money"]]
                       
                       