module Haskeroids.State (
    GameState(..),
    initialGameState
    ) where

import Haskeroids.Player

data GameState = GameState { statePlayer :: Player }

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

initialPlayerState :: Player
initialPlayerState = Player (400, 300)