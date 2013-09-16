module Testbed where

import Shape

-- Init sample model
rect1 = Rectangle "rect" (Position 10 10) (Size 50 50)
model = [rect1]

-- Same actions
--move10 :: (Shape a) -> (Shape a)
move10 shape = move shape 10 10
