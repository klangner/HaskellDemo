module Old.Shape where

-- Base types
data Position = Position Float Float deriving (Show, Eq)
data Size = Size Float Float deriving (Show, Eq)
type Name = String

-- Rectangle
data Shape = Rectangle Name Position Size 
           | Circle Name Position Float
           deriving (Show, Eq)

move :: Shape -> Float -> Float -> Shape
move (Rectangle name (Position x y) size) dx dy = Rectangle name (Position (x+dx) (y+dy)) size
move (Circle name (Position x y) r) dx dy = Circle name (Position (x+dx) (y+dy)) r

resize :: Shape -> Float -> Shape
resize (Rectangle name pos (Size w h)) scale = Rectangle name pos (Size (w*scale) (h*scale))
resize (Circle name pos r) scale = Circle name pos (r*scale)


-- Pseudo partial functions
moveAction :: Float -> Float -> (Shape -> Shape)
moveAction dx dy = \s -> move s dx dy

resizeAction :: Float -> (Shape -> Shape)
resizeAction factor = \s -> resize s factor

type Model = [Shape]
type Action = Shape -> Shape

-- Execute action on shape
applyAction :: Action -> Shape -> Shape
applyAction a s = a s

-- Execute all action if name is correct
--executeActions :: Shape -> [(Name, Action)] -> Shape
--executeActions shapes actions = shapes


