module Haskeroids.Asteroid (
    Asteroid,
    newAsteroid,
    updateAsteroid,
    collideAsteroids,
    asteroidAlive
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render
import Haskeroids.Collision

type Hitpoints = Int
data Size = Small|Medium|Large deriving (Ord, Eq)
data Asteroid = Asteroid {
    asteroidSize :: Size,
    asteroidBody :: Body,
    asteroidHits :: Hitpoints }

instance LineRenderable Asteroid where
    interpolatedLines f (Asteroid sz b _) = map (transform b') $ asteroidLines sz
        where b' = interpolatedBody f b

instance Collider Asteroid where
    collisionCenter (Asteroid _ b _)  = bodyPos b
    collisionRadius (Asteroid sz _ _) = radius sz
    collisionLines = interpolatedLines 0

-- | Initialize a new asteroid with the given position, velocity and rotation
newAsteroid :: Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid pos v r = Asteroid Large (Body pos 0 v r pos 0) 16

-- | Update an asteroid's position
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b hp) = Asteroid sz (updateBody b) hp

damageAsteroid :: Asteroid -> Asteroid
damageAsteroid a = a {asteroidHits = (asteroidHits a) - 1}

-- | Collide an asteroid against multiple colliders and return a new modified
--   asteroid and a list of remaining colliders.
collideAsteroid :: Collider c => [c] -> Asteroid -> ([c], Asteroid)
collideAsteroid cs a = foldr go ([], a) cs
    where go c (cs, a) = if collides c a
            then (cs, damageAsteroid a)
            else (c:cs, a)

-- | Collide a list of asteroids against a list of other colliders
--   Returns the remaining colliders and damaged asteroids.
collideAsteroids :: Collider c => [c] -> [Asteroid] -> ([c], [Asteroid])
collideAsteroids cs = foldr go (cs,[])
    where go a (cs,as) = (cs', a':as)
            where (cs',a') = collideAsteroid cs a

asteroidAlive :: Asteroid -> Bool
asteroidAlive = (0<).asteroidHits

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
asteroidLines = pointsToSegments . pts
    where pts sz  = polarPoints (numVertices sz) (radius sz)
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/(fromIntegral s)
