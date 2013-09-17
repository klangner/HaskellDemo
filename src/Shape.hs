module Shape where

class Shape a where
    move :: a -> Float -> Float -> a
    resize :: a -> Float -> a

-- Base types
data Position = Position Float Float deriving (Show)
data Size = Size Float Float deriving (Show)

-- Rectangle
data Rectangle = Rectangle Position Size deriving (Show)
instance Shape Rectangle where
    move (Rectangle (Position x y) size) dx dy = Rectangle (Position (x+dx) (y+dy)) size
    resize (Rectangle pos (Size w h)) scale = Rectangle pos (Size (w*scale) (h*scale))

-- Circle
data Circle = Circle Position Float deriving (Show)
instance Shape Circle where
    move (Circle (Position x y) r) dx dy = Circle (Position (x+dx) (y+dy)) r
    resize (Circle pos r) scale = Circle pos (r*scale)

{-
-- Curred actions
moveAction :: Shape a => Float -> Float -> (a -> a)
moveAction dx dy = \s -> move s dx dy

resizeAction :: Shape a => Float -> (a -> a)
resizeAction factor = \s -> resize s factor
-}
