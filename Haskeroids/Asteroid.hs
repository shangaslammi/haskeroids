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

-- | Initialize a new asteroid with the given position, velocity and rotation        
newAsteroid :: Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid pos v r = Asteroid Large $ Body pos 0 v r pos 0

-- | Update an asteroid's position
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b) = Asteroid sz $ updateBody b

-- | Get the radius for an asteroid size
radius :: Size -> Float
radius Small  = 14
radius Medium = 28
radius Large  = 56

-- | Get the number of vertices for an asteroid size
numVertices :: Size -> Int
numVertices Small  = 6
numVertices Medium = 8
numVertices Large  = 12

-- | Get the line segments for an asteroid size
asteroidLines sz = pointsToSegments $ pts sz
    where pts sz  = polarPoints (numVertices sz) (radius sz)
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/(fromIntegral s)
