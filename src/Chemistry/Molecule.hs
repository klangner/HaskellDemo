module Chemistry.Molecule ( Molecule
                          ) where

import Chemistry.Element

data Molecule = Molecule [(Element, Int)] deriving Show
