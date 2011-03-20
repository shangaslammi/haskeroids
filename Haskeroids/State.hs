module Haskeroids.State (
    GameState(..),
    initialGameState,
    ) where

import Haskeroids.Player
import Haskeroids.Asteroid
import Haskeroids.Geometry
import Haskeroids.Geometry.Body (initBody)
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard (Keyboard)

-- | Data type for tracking game state
data GameState = GameState {
    statePlayer    :: Player,
    stateAsteroids :: [Asteroid]
    }

instance LineRenderable GameState where
    interpolatedLines f (GameState p a) = plines ++ alines
        where plines = interpolatedLines f p
              alines = concatMap (interpolatedLines f) a

instance Tickable GameState where
    tick = tickState
    
-- | Generate the initial game state
initialGameState :: GameState
initialGameState = GameState {
    statePlayer    = initialPlayerState,
    stateAsteroids = [
        newAsteroid (20,50) (1.5,0.7) (-0.02),
        newAsteroid (700, 10) (-1, 0.4) (-0.015)]
    }

-- | Initial state for the player ship at center of the screen
initialPlayerState :: Player
initialPlayerState = Player $ initBody (400,300)

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> GameState
tickState kb s@(GameState pl a) = s {
    statePlayer    = tick kb pl,
    stateAsteroids = map updateAsteroid a
    }
