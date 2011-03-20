module Haskeroids.Asteroid (
    Asteroid,
    newAsteroid,
    updateAsteroid
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render

data Size = Small|Medium|Large deriving (Ord, Eq)
data Asteroid = Asteroid Size Body

instance LineRenderable Asteroid where
    interpolatedLines f (Asteroid sz b) = map (transform b') $ asteroidLines sz
        where b' = interpolatedBody f b
    
newAsteroid :: Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid pos v r = Asteroid Large $ Body pos 0 v r pos 0

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b) = Asteroid sz $ updateBody b

asteroidLines sz = pointsToSegments $ pts sz
    where pts Small  = polarPoints 6 14
          pts Medium = polarPoints 8 28
          pts Large  = polarPoints 12 56
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/s