module Haskeroids.Asteroid (
    Asteroid,
    newAsteroid,
    updateAsteroid
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body (Body(..), updateBody)

data Size = Small|Medium|Large deriving (Ord, Eq)
data Asteroid = Asteroid Size Body

newAsteroid :: Vec2 -> Vec2 -> Asteroid
newAsteroid pos v = Asteroid Large $ Body pos 0 v 0.05 pos 0

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b) = Asteroid sz $ updateBody b