module Haskeroids.State (
    GameState(..),
    initialGameState
    ) where

import Haskeroids.Player
import Haskeroids.Util
import Haskeroids.Render (LineRenderable(..))

data GameState = GameState { statePlayer :: Player }

instance LineRenderable GameState where
    lineSegments = stateLines

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

initialPlayerState :: Player
initialPlayerState = Player (400, 300)

stateLines :: GameState -> [LineSegment]
stateLines = lineSegments . statePlayer