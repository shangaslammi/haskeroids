module Haskeroids.State (
    GameState(..),
    initialGameState,
    ) where

import Data.List (partition)
    
import Haskeroids.Player
import Haskeroids.Bullet
import Haskeroids.Asteroid
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard (Keyboard)

-- | Data type for tracking game state
data GameState = GameState {
    statePlayer    :: Player,
    stateAsteroids :: [Asteroid],
    stateBullets   :: [Bullet]
    }

instance LineRenderable GameState where
    interpolatedLines f (GameState p a b) = plines ++ alines ++ blines
        where plines = interpolatedLines f p
              alines = concatMap (interpolatedLines f) a
              blines = concatMap (interpolatedLines f) b

instance Tickable GameState where
    tick = tickState

-- | Generate the initial game state
initialGameState :: GameState
initialGameState = GameState {
    statePlayer    = initPlayer,
    stateAsteroids = [
        newAsteroid Large (20,50) (1.5,0.7) (-0.02),
        newAsteroid Large (700, 10) (-1, 0.4) (-0.015)],
    stateBullets   = []
    }

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> GameState
tickState kb s@(GameState pl a b) = s {
    statePlayer    = collidePlayer a' p',
    stateAsteroids = aa,
    stateBullets   = b''
    }
    where  (b'', a'') = collideAsteroids b' a'
           (aa, ad)   = partition asteroidAlive a''

           p' = tick kb pl
           a' = map updateAsteroid a
           b' = filter bulletActive . map updateBullet $ newBullets
           newBullets = case (playerBullet p') of
                Nothing -> b
                Just x  -> x:b
