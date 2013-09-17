module Testbed where

import Shape

-- Init sample model
model :: [Shape]
model = [
        Rectangle "rect1" (Position 10 10) (Size 50 50),
        Circle "c1" (Position 20 30) 5
        ]

actions :: [(Name, Shape -> Shape)]
actions = [
            ("rect", moveAction 10 10), 
            ("c1", moveAction 1 4)
          ]
