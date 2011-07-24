module Haskeroids.State
    ( GameState(..)
    , initialGameState
    , tickStateIO
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
    interpolatedLines f (GameState p a b s _) = pls ++ als ++ bls ++ sls where
        pls = interpolatedLines f p
        als = concatMap (interpolatedLines f) a
        bls = concatMap (interpolatedLines f) b
        sls = interpolatedLines f s

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

-- | Function that combines pure calculations and initializing random
--   objects in the IO monad
tickStateIO :: Keyboard -> GameState -> IO GameState
tickStateIO kb s = do
    (s',np) <- fmap (runParticleGen . tickState kb) $ initNewAsteroids s
    ps <- initNewParticles np $ stateParticles s'
    return s' { stateParticles = ps }

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> ParticleGen GameState
tickState kb s@(GameState pl a b p _) = do
    let a' = map updateAsteroid a
    pl' <- tickPlayer kb pl >>= collidePlayer a'

    let b' = filter bulletActive . map updateBullet $ case playerBullet pl' of
                Nothing -> b
                Just x  -> x:b
    (b'', a'') <- collideAsteroids b' a'

    let (aa, ad) = partition asteroidAlive a''
    na <- mapM spawnNewAsteroids ad

    return s
      { statePlayer    = pl'
      , stateAsteroids = aa
      , stateBullets   = b''
      , stateParticles = p
      , newAsteroids   = concat na
      }
