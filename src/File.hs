import System.IO


-- Convert json string to data
tojson :: String -> Int
tojson x = 1

readRecords h = do
    contents <- hGetContents h
    return (map tojson (lines contents))

main :: IO ()
main = do  
    handle <- openFile "../data/test.log" ReadMode  
    records <- readRecords handle
    putStr (show (length records))
    hClose handle  

max' :: Ord a => [a] -> a
max' [x] = x
max' (x:xs) = if x > y then x else y
             where
                y = max' xs
