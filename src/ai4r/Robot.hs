module Robot where

type Landmarks = [(Float, Float)]
type Angle = Float
data Robot = Robot Float Float Angle deriving(Show)

forwardNoise = 5.0
turnNoise    = 0.1
senseNoise   = 5.0

landmarks :: Landmarks
landmarks = [(20.0, 20.0), (80.0, 80.0), (20.0, 80.0), (80.0, 20.0)]

turn :: Robot -> Angle -> Robot
turn (Robot x y d) a = Robot x y (d+a)

move :: Robot -> Float -> Robot
move (Robot x y d) dist = Robot (x+dx) (y+dy) d
                            where dx = dist * (cos d)
                                  dy = dist * (sin d)

sense :: Robot -> Landmarks -> [Float]
sense r [] = []
sense (Robot x y d) ((lx,ly):ls) = (sqrt (dx^2 + dy^2):rest)
                                    where dx = lx-x
                                          dy = ly-y
                                          rest = sense (Robot x y d) ls


-- For testing
myrobot = Robot 30 50 (pi/2)
