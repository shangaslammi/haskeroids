module Haskeroids.Asteroid (
    Asteroid,
    Size(..),
    newAsteroid,
    updateAsteroid,
    spawnNewAsteroids,
    collideAsteroids,
    asteroidAlive
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render
import Haskeroids.Collision

import Data.List (replicate)
import System.Random (randomRIO)

type Hitpoints = Int
data Size = Small|Medium|Large deriving (Ord, Eq, Enum)
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
newAsteroid :: Size -> Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid sz pos v r = Asteroid sz (Body pos 0 v r pos 0) (maxHits sz)

-- | Update an asteroid's position
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b hp) = Asteroid sz (updateBody b) hp

-- | Reduce asteroid hitpoints by one
damageAsteroid :: Asteroid -> Asteroid
damageAsteroid a = a {asteroidHits = (asteroidHits a) - 1}

-- | Check if the asteroid still has hitpoints left
asteroidAlive :: Asteroid -> Bool
asteroidAlive = (0<).asteroidHits

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

-- | Get the radius for an asteroid size
radius :: Size -> Float
radius Small  = 14
radius Medium = 32
radius Large  = 70

maxHits Small  = 4
maxHits Medium = 8
maxHits Large  = 16

-- | Spawn random asteroids
spawnNewAsteroids :: Asteroid -> [IO Asteroid]
spawnNewAsteroids (Asteroid sz b _)
    | sz == Small = []
    | otherwise   = replicate 3 $ randomAsteroid (pred sz) (bodyPos b)

randomAsteroid :: Size -> Vec2 -> IO Asteroid
randomAsteroid sz pos = do
    dx <- randomRIO (-r*1.0, r*1.0)
    dy <- randomRIO (-r*1.0, r*1.0)
    vx <- randomRIO (-40.0/r, 40.0/r)
    vy <- randomRIO (-40.0/r, 40.0/r)
    r  <- randomRIO (-3.0/r, 3.0/r)
    return $ newAsteroid sz (pos /+/ (dx,dy)) (vx,vy) r
    where r = radius sz

-- | Get the number of vertices for an asteroid size
numVertices :: Size -> Int
numVertices Small  = 9
numVertices Medium = 13
numVertices Large  = 17

-- | Get the line segments for an asteroid size
asteroidLines = pointsToSegments . pts
    where pts sz  = polarPoints (numVertices sz) (radius sz)
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/(fromIntegral s)
