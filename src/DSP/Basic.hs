module DSP.Basic where

type Signal = [Float]
type Input = Integer -> Float

-- Scale signal
scale :: Float -> Input -> Integer -> Float
scale a input n = a*(input n)

-- Add signals
add :: Input -> Input -> Integer -> Float
add a b n = (a n) + (b n)


-- Delta (impulse) function
delta :: Integer -> Float
delta 0 = 1
delta _ = 0

-- Delta (impulse) function
step :: Integer -> Float
step n
   | n > 0 = 1
   | otherwise = 0

