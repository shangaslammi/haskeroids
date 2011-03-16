module Haskeroids.Player (Player(..)) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Transform
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick

-- | Data type for tracking current player state
data Player = Player { playerBody :: Body }

instance LineRenderable Player where
    lineSegments (Player {playerBody = b}) = map (transform b) $ shipLines

instance Tickable Player where
    tick (Player {playerBody = b@(Body {bodyAngle = a})}) = Player $ b {
        bodyAngle = a + 0.1 }
    
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
