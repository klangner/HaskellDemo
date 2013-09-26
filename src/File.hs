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
