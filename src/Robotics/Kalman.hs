module Robotics.Kalman where


type Gaussian = (Float, Float)

update :: Gaussian -> Gaussian -> Gaussian
update (m1,d1) (m2,d2) = (m, d)
                        where m = (m1*d2 + m2*d1) / (d1 + d2)
                              d = 1 / (1/d1 + 1/d2)

predict :: Gaussian -> Gaussian -> Gaussian
predict (m1,d1) (m2,d2) = (m1+m2, d1+d2)


measurements :: [Float]
measurements = [5, 6, 7, 9, 10]
motions :: [Float]
motions = [1, 1, 2, 1, 1]
measurementSigma :: Float
measurementSigma = 4
motionSigma :: Float
motionSigma = 2
mu :: Float
mu = 0
sig :: Float
sig = 10000

apply :: [Float] -> [Float] -> Gaussian -> Gaussian
apply [] _ g = g
apply _ [] g = g
apply (x:xs) (y:ys) g = apply xs ys g2
                        where g2 = predict (y,measurementSigma) g1
                              g1 = update (x,motionSigma) g
