module Haskeroids.State
    ( GameState(..)
    , initialGameState
    , tickState
    ) where

import Data.List (partition)
import Control.Monad (replicateM)

import Haskeroids.Player
import Haskeroids.Bullet
import Haskeroids.Asteroid
import Haskeroids.Particles
import Haskeroids.Random
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard (Keyboard)
import Haskeroids.Text
import Haskeroids.Text.Font

-- | Data type for tracking game state
data GameState = GameState
    { statePlayer    :: Player
    , stateAsteroids :: [Asteroid]
    , stateBullets   :: [Bullet]
    , stateParticles :: ParticleSystem
    , stateTexts     :: [Text]
    , stateRandom    :: RandomGen
    , stateFont      :: Font
    } | NewGame Font

instance LineRenderable GameState where
    interpolatedLines f (GameState p a b s _ _ _)
        = pls ++ als ++ bls ++ sls where
        pls = interpolatedLines f p
        als = concatMap (interpolatedLines f) a
        bls = concatMap (interpolatedLines f) b
        sls = interpolatedLines f s

-- | Generate the initial game state
initialGameState :: Font -> GameState
initialGameState = NewGame

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> GameState
tickState kb (NewGame f) = GameState
    { statePlayer    = initPlayer
    , stateAsteroids = a
    , stateBullets   = []
    , stateParticles = initParticleSystem
    , stateTexts     = []
    , stateRandom    = g
    , stateFont      = f
    } where
    (a, g) = runRandom (replicateM 3 genInitialAsteroid) $ initRandomGen 0

tickState kb s@(GameState pl a b p t g f) = s'
    { stateParticles = p'
    , stateRandom    = g'
    } where
    np'      = initNewParticles np (stateParticles s')
    (p', g') = runRandom np' $ stateRandom s'
    (s',np)  = runParticleGen $ do
        let a' = map updateAsteroid a
        pl' <- tickPlayer kb pl >>= collidePlayer a'

        let b' = updateBullets $ case playerBullet pl' of
                    Nothing -> b
                    Just x  -> x:b
        (b'', a'') <- collideAsteroids b' a'

        let (aa, ad) = partition asteroidAlive a''
        na <- mapM spawnNewAsteroids ad

        let (na', g') = runRandom (sequence $ concat na) g

        return s
          { statePlayer    = pl'
          , stateAsteroids = na' ++ aa
          , stateBullets   = b''
          , stateParticles = tickParticles p
          , stateRandom    = g'
          , stateFont      = f
          }
