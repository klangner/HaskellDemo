module Basic where

type Signal = [Float]

-- Add 2 signals
add :: Signal -> Signal -> Signal
add [] _ = []
add _ [] = []
add (a:as) (b:bs) = (a+b:add as bs)

-- Delay signal
delay :: Signal -> Integer -> Signal
delay [] _ = []
delay as 0 = as
delay as n = (0: delay as (n-1))

-- Scale signal
scale :: Signal -> Float -> Signal
scale [] _ = []
scale (a:as) f = (a*f:scale as f)


-- Test data
impulse :: [Float]
impulse = [0, 0, 0, 1, 0, 0, 0]
s2 :: [Float]
s2 = [1, 2, 3, 4, 5, 6]
step :: [Float]
step = [0, 0, 0, 1, 1, 1, 1]

