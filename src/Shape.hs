module Shape where

class Shape a where
    move :: a -> Float -> Float -> a
    scale :: a -> Float -> a

-- Base types
type Name = String
data Position = Position Float Float deriving (Show)
data Size = Size Float Float deriving (Show)

-- Shapes
data Rectangle = Rectangle Name Position Size deriving (Show)
instance Shape Rectangle where
    move (Rectangle name (Position x y) size) dx dy = Rectangle name (Position (x+dx) (y+dy)) size
    scale (Rectangle name pos (Size w h)) scale = Rectangle name pos (Size (w*scale) (h*scale))
    
data Circle = Circle  Name Position Float deriving (Show)
instance Shape Circle where
    move (Circle name (Position x y) r) dx dy = Circle name (Position (x+dx) (y+dy)) r
    scale (Circle name pos r) scale = Circle name pos (r*scale)

