module Haskeroids.Player (Player(..)) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard (isKeyDown)
import Haskeroids.Controls

-- | Data type for tracking current player state
data Player = Player { playerBody :: Body }

instance LineRenderable Player where
    lineSegments (Player b) = map (transform b) $ shipLines
    
    interpolatedLines f (Player b) = map (transform b') $ shipLines
        where b' = interpolatedBody f b

instance Tickable Player where
    tick kb (Player b) = Player $ updatePlayerBody turn acc b
        where turn | key turnLeft  = -0.12
                   | key turnRight = 0.12
                   | otherwise     = 0
              
              acc | key thrust = 0.5
                  | otherwise  = 0
                  
              key = isKeyDown kb

-- | Update the player ship with the given turn rate and acceleration
updatePlayerBody :: Float -> Float -> Body -> Body
updatePlayerBody turn acc = updateBody . damping 0.96 . accForward acc . rotate turn
    
-- | Constant for the ship size
shipSize = 12.0 :: Float

-- | List of lines that make up the ship hull
shipLines :: [LineSegment]
shipLines = pointsToSegments points
    where points = [polar shipSize      0,
                    polar shipSize      (0.7*pi),
                    polar (shipSize*0.2) pi,
                    polar shipSize      (1.3*pi),
                    polar shipSize      0]
