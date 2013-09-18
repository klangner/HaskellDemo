
import qualified Data.Map as Map
import Tests
import Shape

-- Init sample model
model :: Model
model = [
        Rectangle "rect1" (Position 10 10) (Size 50 50),
        Circle "c1" (Position 20 30) 5
        ]

keyBindings :: Map.Map Char Action
keyBindings =  Map.fromList [
            ('w', moveAction 10 0), 
            ('s', moveAction (-10) 0),
            ('a', moveAction 0 (-10)),
            ('d', moveAction 0 10)
          ]


actionLoop :: Maybe Action -> IO ()
actionLoop Nothing = return ()
actionLoop (Just action) = do 
    print (map action model)
    c <- getChar
    actionLoop (Map.lookup c keyBindings)



main :: IO ()
main = do
    c <- getChar
    actionLoop (Map.lookup c keyBindings)

