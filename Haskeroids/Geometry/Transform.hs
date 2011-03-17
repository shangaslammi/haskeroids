module Haskeroids.Geometry.Transform (
    Body (..),
    transform
    ) where

import Haskeroids.Geometry

-- | Data type that contains the position and orientation of a rigid body
data Body = Body {
    bodyPos   :: Vec2,
    bodyAngle :: Float
    }

-- | Transform a line segment according to body position and orientation
transform :: Body -> LineSegment -> LineSegment
transform (Body pos a) = applyXform $ (translatePt pos) . (rotatePt a)

-- | Translate a point
translatePt :: Vec2 -- ^ (x,y) delta
            -> Vec2 -- ^ original point
            -> Vec2 -- ^ translated point
translatePt (x,y) (x',y') = (x+x', y+y')

-- | Rotate a point around the origo
rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * (cos a) - y * (sin a)
          y' = x * (sin a) + y * (cos a)

-- | Apply a point transformation function to a line segment
applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = (LineSegment (f p, f p'))
