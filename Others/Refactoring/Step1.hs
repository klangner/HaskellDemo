module Refactoring.Step1 where

type Name = String
type Title = String
type Price = Float
type Days = Int

data MovieType = Regular | Childrens | NewRelease

data MovieInfo = Movie MovieType Title
data Customer = Customer Name
data Rental = Rental MovieInfo Customer Days

data Status = Status { movies :: [MovieInfo]
                     , customers :: [Customer]
                     , rentals :: [Rental] }


-- Calculate rental price
statement :: Rental -> Price
statement (Rental (Movie t _) _ n) = rentCost t n

-- Rental cost depends on movie type and number of days
rentCost :: MovieType -> Days -> Price
rentCost Regular n = 2.0 + fromIntegral (min 0 (n - 2)) * 1.5
rentCost Childrens n = 1.5 + fromIntegral (min 0 (n - 3)) * 1.5 
rentCost NewRelease n = 3.0 + fromIntegral n  

-- Calculate bonus points
calcBonusPoints :: Rental -> Int
calcBonusPoints (Rental (Movie NewRelease _) _ n) = if n > 0 then 1 else 0
calcBonusPoints _ = 0

-- Build rent summary
buildRentSummary :: Rental -> String
buildRentSummary (Rental (Movie _ t) _ n) = "Your rented " ++ t ++ " for " ++ show n ++ "days." 


-- Possible functions:
-- add movie X to the store
-- rent movie X to customer A
-- return movie X
-- is movie X available?
-- who owns movie X?
-- What movies has customer A?