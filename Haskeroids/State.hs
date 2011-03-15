module Haskeroids.State (
    GameState(..),
    initialGameState,
    renderGameState
    ) where

import Haskeroids.Player
import Haskeroids.Util
import Haskeroids.Render (LineRenderable(..))

-- | Data type for tracking game state
data GameState = GameState { statePlayer :: Player }

instance LineRenderable GameState where
    lineSegments = stateLines

-- | Generate the initial game state
initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

-- | Render the given game state
renderGameState :: GameState -> IO ()
renderGameState = render
    
-- | Initial state for the player ship at center of the screen
initialPlayerState :: Player
initialPlayerState = Player (400, 300)

-- | List of all renderable lines in the given state
stateLines :: GameState -> [LineSegment]
stateLines = lineSegments . statePlayer