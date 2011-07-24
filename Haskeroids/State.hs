module Haskeroids.State
    ( GameState(..)
    , initialGameState
    , initNewAsteroids
    , tickState
    ) where

import Data.List (partition, replicate)

import Haskeroids.Player
import Haskeroids.Bullet
import Haskeroids.Asteroid
import Haskeroids.Particles
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard (Keyboard)

-- | Data type for tracking game state
data GameState = GameState
    { statePlayer    :: Player
    , stateAsteroids :: [Asteroid]
    , stateBullets   :: [Bullet]
    , stateParticles :: ParticleSystem
    , newAsteroids   :: [RandomAsteroid]
    }

instance LineRenderable GameState where
    interpolatedLines f (GameState p a b _ _) = plines ++ alines ++ blines where
        plines = interpolatedLines f p
        alines = concatMap (interpolatedLines f) a
        blines = concatMap (interpolatedLines f) b

-- | Generate the initial game state
initialGameState :: GameState
initialGameState = GameState
    { statePlayer    = initPlayer
    , stateAsteroids = []
    , stateBullets   = []
    , stateParticles = initParticleSystem
    , newAsteroids   = replicate 3 genInitialAsteroid
    }

-- | Initialize new random asteroids in the IO monad
initNewAsteroids :: GameState -> IO GameState
initNewAsteroids st = do
    n <- sequence $ newAsteroids st
    return st { stateAsteroids = n ++ stateAsteroids st }

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> GameState
tickState kb s@(GameState pl a b p _) = s
    { statePlayer    = collidePlayer a' p'
    , stateAsteroids = aa
    , stateBullets   = b''
    , stateParticles = p
    , newAsteroids   = concatMap spawnNewAsteroids ad
    } where
        (b'', a'') = collideAsteroids b' a'
        (aa, ad)   = partition asteroidAlive a''

        p' = tickPlayer kb pl
        a' = map updateAsteroid a
        b' = filter bulletActive . map updateBullet $ case playerBullet p' of
                Nothing -> b
                Just x  -> x:b
