module Haskeroids.Asteroid
    ( Asteroid
    , RandomAsteroid
    , Size(..)
    , genInitialAsteroid
    , updateAsteroid
    , spawnNewAsteroids
    , collideAsteroids
    , asteroidAlive
    ) where

import Haskeroids.Collision
import Haskeroids.Events
import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Particles
import Haskeroids.Random
import Haskeroids.Render

import Control.Monad (replicateM_)
import Data.List (sort)
import Data.Foldable (foldrM)

type Hitpoints = Int
data Size = Small|Medium|Large deriving (Ord, Eq, Enum)
data Asteroid = Asteroid
    { asteroidSize  :: Size
    , asteroidBody  :: Body
    , asteroidHits  :: Hitpoints
    , asteroidLines :: [LineSegment]
    }

type RandomAsteroid = Random Asteroid

instance LineRenderable Asteroid where
    interpolatedLines f (Asteroid sz b _ lns) = map (transform b') lns where
        b' = interpolatedBody f b

instance Collider Asteroid where
    collisionCenter = bodyPos . asteroidBody
    collisionRadius = radius . asteroidSize
    collisionLines  = interpolatedLines 0

-- | Radius for an asteroid
radius :: Size -> Float
radius Small  = 14
radius Medium = 32
radius Large  = 70

-- | Hitpoints for an asteroid
maxHits :: Size -> Int
maxHits Small  = 3
maxHits Medium = 6
maxHits Large  = 9

-- | Number of vertices for an asteroid
numVertices :: Size -> Int
numVertices Small  = 9
numVertices Medium = 13
numVertices Large  = 17

-- | Initialize a new asteroid with the given position, velocity and rotation
initAsteroid :: Size -> Vec2 -> Vec2 -> Float -> [LineSegment] -> Asteroid
initAsteroid sz pos v r = Asteroid sz (initBody pos 0 v r) (maxHits sz)

-- | Update an asteroid's position
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a = a { asteroidBody = updateBody $ asteroidBody a }

-- | Reduce asteroid hitpoints by one
damageAsteroid :: Asteroid -> Asteroid
damageAsteroid a = a {asteroidHits = asteroidHits a - 1}

-- | Check if the asteroid still has hitpoints left
asteroidAlive :: Asteroid -> Bool
asteroidAlive = (0<).asteroidHits

-- | Collide an asteroid against multiple colliders and return a new modified
--   asteroid and a list of remaining colliders.
collideAsteroid :: Collider c => [c] -> Asteroid -> Events ([c], Asteroid)
collideAsteroid cs a = foldrM go ([], a) cs where
    go c (cs, a)
        | collides c a = collisionParticles c >> return (cs, damageAsteroid a)
        | otherwise    = return (c:cs, a)

-- | Collide a list of asteroids against a list of other colliders
--   Returns the remaining colliders and damaged asteroids.
collideAsteroids :: Collider c => [c] -> [Asteroid] -> Events ([c], [Asteroid])
collideAsteroids cs = foldrM go (cs,[]) where
    go a (cs,as) = do
        (cs',a') <- collideAsteroid cs a
        return (cs', a':as)

-- | Spawn random asteroids
spawnNewAsteroids :: Asteroid -> Events ()
spawnNewAsteroids a@(Asteroid sz b _ _) =
    explosionParticles a >> new where
    new
        | sz == Small = return ()
        | otherwise   = replicateM_ 3 $ newAsteroid a
        where a = randomAsteroid (pred sz) (bodyPos b)

explosionParticles :: Asteroid -> Events ()
explosionParticles (Asteroid sz b _ _) = newParticles n NewParticle
    { npPosition  = bodyPos b
    , npRadius    = radius sz / 2.0
    , npDirection = 0
    , npSpread    = 2*pi
    , npSpeed     = (3.0, 6.0)
    , npLifeTime  = (15, 40)
    , npSize      = (1,3)
    } where
        n = round $ radius sz

randomAsteroid :: Size -> Vec2 -> RandomAsteroid
randomAsteroid sz pos = do
    let r = radius sz

    (dx,dy) <- randomPair r
    (vx,vy) <- randomPair (40.0/r)
    rot     <- randomBracket (3.0/r)
    lns     <- genAsteroidLines sz

    return $ initAsteroid sz (pos /+/ (dx,dy)) (vx,vy) rot lns


-- | Generate an initial asteroid in the level
genInitialAsteroid :: RandomAsteroid
genInitialAsteroid = randomElliptical xb yb >>= randomAsteroid Large where
    xb = (140, 400)
    yb = (140, 300)

-- | Generate the line segments for an asteroid size
genAsteroidLines :: Size -> Random [LineSegment]
genAsteroidLines sz = do
    let numPts = numVertices sz
        r      = radius sz

    radii  <- nrandomR numPts (r*0.5, r)
    angvar <- nrandomR numPts (-0.01*pi, 0.01*pi)

    let step   = 2.0*pi/(fromIntegral numPts + 1)
        angles = sort $ zipWith (+) angvar [0.0,step..2.0*pi]
        points = zipWith polar radii angles

    return $ pointsToSegments $ points ++ [head points]
