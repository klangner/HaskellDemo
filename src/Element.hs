module Element where

data Element =  Rectangle {left::Int, top::Int, width::Int, height::Int}
             | Circle {x::Int, y::Int, radius::Int}
             | Arrow {x1::Int, y1::Int, x2::Int, y2::Int}
                deriving Show

move :: Element -> Int -> Int -> Element
move (Circle x y r) dx dy = Circle (x+dx) (y+dy) r
move (Rectangle x y w h) dx dy = Rectangle (x+dx) (y+dy) w h
move (Arrow x1 y1 x2 y2) dx dy = Arrow (x1+dx) (y1+dy) (x2+dx) (y2+dy)
