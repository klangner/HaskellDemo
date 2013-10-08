module Localization where

type WorldMap = [String]
type Model = [Float]

location :: Model
-- location = [0.2, 0.2, 0.2, 0.2, 0.2]
location = [0, 1, 0, 0, 0]

world :: WorldMap
world = ["green", "red", "red", "green", "green"]

-- Sensor reading probability distribution
sHit = 0.6
sMiss = 0.2
-- Movement propbability distribution
mExact = 0.8
mOvershoot = 0.1
mUndershoot = 0.1

applySensor :: String -> WorldMap -> Model -> Model
applySensor _ _ [] = []
applySensor _ [] _ = []
applySensor d (w:ws) (x:xs)
    | d == w = (sHit*x:applySensor d ws xs)
    | otherwise = (sMiss*x:applySensor d ws xs)

normalize :: Model -> Model
normalize m = map (* c) m
              where c = 1 / (sum m)

shift :: Int -> [a] -> [a]
shift d [] = []
shift d xs = bs ++ as
            where (as,bs) = splitAt p xs
                  p = len - (mod d len)
                  len = length xs

move :: Int -> Model -> Model
move d xs = [a*mUndershoot + b*mExact + c*mOvershoot | (a,b,c) <- ss]
            where ss = shift d zs
                  zs = zip3 (shift (-1) xs) xs (shift 1 xs)

repeat2 :: Int -> (a -> a) -> a -> a
repeat2 1 f v = f v
repeat2 n f v = f (repeat2 (n-1) f v)
