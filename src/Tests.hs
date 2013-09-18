module Testbed where

import Test.QuickCheck
import Shape

-- Init sample model
model :: Model
model = [
        Rectangle "rect1" (Position 10 10) (Size 50 50),
        Circle "c1" (Position 20 30) 5
        ]

actions :: [(Name, Action)]
actions = [
            ("rect", moveAction 10 10), 
            ("rect", moveAction 20 0),
            ("c1", moveAction 1 4)
          ]

-- Test
prop_move :: Action -> Shape -> Shape -> Bool
prop_move a s expected = shape == expected
    where shape = applyAction a s


test1 = prop_move (moveAction 10 10) (Rectangle "rect1" (Position 10 10) (Size 50 50)) (Rectangle "rect1" (Position 20 20) (Size 50 50))
test2 = prop_move (moveAction 10 10) (Circle "c1" (Position 10 10) 5) (Circle "c1" (Position 20 20) 5)

