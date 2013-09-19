
import qualified Data.Map as Map
import Tests
import Shape

-- Init sample model
baseModel :: Model
baseModel = [
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

key2action :: Char -> Maybe Action
key2action c = Map.lookup c keyBindings


actionLoop :: Maybe Action -> Model -> IO ()
actionLoop Nothing _ = return ()
actionLoop (Just action) model = do 
    print newModel
    c <- getChar
    actionLoop (key2action c) newModel
    where
        newModel = map action model
        


main :: IO ()
main = do
    c <- getChar
    actionLoop (key2action c) baseModel

