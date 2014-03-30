module GameOfLife where


-- Console helpers
type Pos = (Int, Int)

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

goto :: Pos -> IO ()
goto (x,y) =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs =  do goto p
                   putStr xs

cls :: IO ()
cls =  putStr "\ESC[2J"

-- Game of life
width :: Int
width = 20

height :: Int
height = 20

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showBoard :: Board -> IO ()
showBoard ps = seqn ([writeat p "O" | p <- ps] ++ [goto (1,1)])

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x-1, y), (x-1, y+1), 
                          (x, y-1), (x, y+1), 
                          (x+1, y-1), (x+1, y), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> Board
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> Board
births b = [(x,y) | x <- [1..width], y <- [1..height], isEmpty b (x,y), liveneighbs b (x,y) == 3]

rmduplicates :: Eq a => [a] -> [a]
rmduplicates [] = []
rmduplicates (x:xs) = (x: rmduplicates (filter (/= x) xs))

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showBoard b
            wait 50000
            life (nextgen b)

wait :: Int -> IO()
wait n = seqn [ return () | _ <- [1..n]]

