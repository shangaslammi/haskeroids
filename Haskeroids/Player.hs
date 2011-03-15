module Haskeroids.Player (Player(..)) where

import Haskeroids.Util
import Haskeroids.Render (LineRenderable(..))

data Player = Player { playerPos :: Pt2 }

instance LineRenderable Player where
    lineSegments _ = shipLines

shipSize :: Float
shipSize = 12.0

shipLines :: [LineSegment]
shipLines = linesToSegments points
    where points = [radial shipSize 0, radial shipSize (0.7*pi),
                    radial (shipSize*0.5) pi, radial shipSize (1.3*pi),
                    radial shipSize 0]
