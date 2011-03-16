module Haskeroids.Player (Player(..)) where

import Haskeroids.Geometry
import Haskeroids.Render (LineRenderable(..))

-- | Data type for tracking current player state
data Player = Player { playerPos :: Vec2 }

instance LineRenderable Player where
    lineSegments p = map (translateLine (playerPos p)) $ shipLines

-- | Constant for player ship size
shipSize = 12.0 :: Float

-- | List of lines that make up the ship hull
shipLines :: [LineSegment]
shipLines = pointsToSegments points
    where points = [radial shipSize 0, radial shipSize (0.7*pi),
                    radial (shipSize*0.2) pi, radial shipSize (1.3*pi),
                    radial shipSize 0]
