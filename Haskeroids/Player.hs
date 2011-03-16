module Haskeroids.Player (Player(..)) where

import Haskeroids.Geometry
import Haskeroids.Render (LineRenderable(..))

-- | Data type for tracking current player state
data Player = Player { playerPos :: Vec2 }

instance LineRenderable Player where
    lineSegments (Player {playerPos = p}) = map (translateLine p) $ shipLines

-- | Constant for player ship size
shipSize = 12.0 :: Float

-- | List of lines that make up the ship hull
shipLines :: [LineSegment]
shipLines = pointsToSegments points
    where points = [polar shipSize      0,
                    polar shipSize      (0.7*pi),
                    polar (shipSize*0.2) pi,
                    polar shipSize      (1.3*pi),
                    polar shipSize      0]
