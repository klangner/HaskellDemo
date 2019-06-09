
import System.Environment
import Algorithms.ThreeSum


main :: IO ()
main =  do
    [f] <- getArgs
    s <- {-# SCC "readFile" #-} readFile f
    let xs = {-# SCC "convert_to_int" #-} map read (lines s) ::[Int]
    putStrLn $ "Number of elements " ++ show (length xs)
    let c = {-# SCC "count_3" #-} count3 xs
    putStrLn $ "Found " ++ show c
    