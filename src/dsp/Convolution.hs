module Convolution where

type Signal = [Double]

-- convolution
conv :: Signal -> Signal -> Signal
conv x h = []


-- Delta (impulse) function
delta = [0, 0, 0, 1, 0, 0, 0]
step = [0, 0, 0, 1, 1, 1, 1]

x = [0, -1, -1.2, 2, 1.4, 1.4, 0.6, 0, -0.7]
h = [1, -0.5, -0.2, -0.1]
