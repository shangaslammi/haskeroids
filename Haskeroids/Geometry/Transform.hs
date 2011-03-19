module Haskeroids.Geometry.Transform (
    Body (..),
    transform,
    rotate,
    damping,
    accForward,
    updateBody,
    initBody,
    ) where

import Haskeroids.Geometry

-- | Data type that contains the position and orientation of a rigid body
data Body = Body {
    bodyPos   :: Vec2,
    bodyAngle :: Float,
    
    bodyVelocity :: Vec2,
    bodyRotation :: Float
    }

-- | Initialize a new rigid body in the given location
initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0,0) 0
    
-- | Accelerate a rigid body according to the given vector
accelerate :: Vec2 -> Body -> Body
accelerate (ax,ay) b = b { bodyVelocity = newVelocity }
    where newVelocity = (ax+vx, ay+vy)
          (vx,vy)     = bodyVelocity b

-- | Accelerate a rigid body according to its current angle
accForward :: Float -> Body -> Body
accForward m b = accelerate (polar m (bodyAngle b)) b
          
-- | Update the position and orientation of a body according to its current
--   velocity and rotation.
updateBody :: Body -> Body
updateBody b = b { bodyPos = pos, bodyAngle = a }
    where pos = bodyPos b   /+/ bodyVelocity b
          a   = bodyAngle b + bodyRotation b

-- | Apply a damping effect to velocity using the given coefficient
damping :: Float -> Body -> Body
damping coefficient b = b { bodyVelocity = coefficient /*/ bodyVelocity b }
          
-- | Rotate a body
rotate :: Float -> Body -> Body
rotate d b@(Body {bodyAngle=a}) = b {bodyAngle = a+d}
    
-- | Transform a line segment according to body position and orientation
transform :: Body -> LineSegment -> LineSegment
transform (Body pos a _ _) = applyXform $ (translatePt pos) . (rotatePt a)

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
