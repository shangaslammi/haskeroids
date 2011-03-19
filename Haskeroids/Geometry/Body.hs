module Haskeroids.Geometry.Body (
    Body (..),
    transform,
    rotate,
    damping,
    accForward,
    updateBody,
    initBody,
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Transform
    
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

-- | Update the position and orientation of a body according to its current
--   velocity and rotation.
updateBody :: Body -> Body
updateBody b = b { bodyPos = pos, bodyAngle = a }
    where pos = bodyPos b   /+/ bodyVelocity b
          a   = bodyAngle b + bodyRotation b

-- | Accelerate a rigid body with the given vector
accelerate :: Vec2 -> Body -> Body
accelerate (ax,ay) b = b { bodyVelocity = newVelocity }
    where newVelocity = (ax+vx, ay+vy)
          (vx,vy)     = bodyVelocity b

-- | Accelerate a rigid body in the direction of its current angle with the
--   given magnitude.
accForward :: Float -> Body -> Body
accForward m b = accelerate (polar m (bodyAngle b)) b
          
-- | Apply a damping effect to velocity using the given coefficient
damping :: Float -> Body -> Body
damping coefficient b = b { bodyVelocity = coefficient */ bodyVelocity b }
          
-- | Rotate a body by the given amount per tick
rotate :: Float -> Body -> Body
rotate nr b = b {bodyRotation = nr}
    
-- | Transform a line segment according to body position and orientation
transform :: Body -> LineSegment -> LineSegment
transform (Body pos a _ _) = applyXform $ (translatePt pos) . (rotatePt a)
