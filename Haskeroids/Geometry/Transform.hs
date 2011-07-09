module Haskeroids.Geometry.Transform where

import Haskeroids.Geometry

-- | Translate a point
translatePt :: Vec2 -- ^ (x,y) delta
            -> Vec2 -- ^ original point
            -> Vec2 -- ^ translated point
translatePt (x,y) (x',y') = (x+x', y+y')

-- | Rotate a point around the origo
rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * cos a - y * sin a
          y' = x * sin a + y * cos a

-- | Apply a point transformation function to a line segment
applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = LineSegment (f p, f p')
