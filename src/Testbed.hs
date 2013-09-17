module Testbed where

import qualified Data.Map as Map
import Shape

-- Types
type Name = String
data MyShape = MyRect Name Rectangle
             | MyCircle Name Circle
                deriving (Show)

-- Init sample model
model :: [MyShape]
model = [
        MyRect "r1" (Rectangle (Position 10 10) (Size 50 50)),
        MyCircle "c1" (Circle (Position 20 30) 5)
        ]

-- Same actions
moveAction :: Float -> Float -> (MyShape -> MyShape)
moveAction dx dy = \s -> case s of
                    MyRect name rect -> MyRect name (move rect dx dy)
                    MyCircle name circle -> MyCircle name (move circle dx dy)

resizeAction :: Float -> (MyShape -> MyShape)
resizeAction factor = \s -> case s of
                    MyRect name rect -> MyRect name (resize rect factor)
                    MyCircle name circle -> MyCircle name (resize circle factor)


actions :: [(Name, MyShape -> MyShape)]
actions = [
            ("rect", moveAction 10 10), 
            ("c1", moveAction 1 4)
          ]

