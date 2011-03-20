module Haskeroids.Geometry.Body (
    Body (..),
    transform,
    rotate,
    damping,
    accForward,
    updateBody,
    initBody,
    interpolatedBody,
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Transform
    
-- | Data type that contains the position and orientation of a rigid body
data Body = Body {
    bodyPos   :: Vec2,
    bodyAngle :: Float,
    
    bodyVelocity :: Vec2,
    bodyRotation :: Float,
    
    prevPos   :: Vec2,
    prevAngle :: Float
    }

-- | Initialize a new rigid body in the given location
initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0,0) 0 pos 0

-- | Update the position and orientation of a body according to its current
--   velocity and rotation.
updateBody :: Body -> Body
updateBody b = b { 
    bodyPos = pos' /+/ wrap,  bodyAngle = a',
    prevPos = pos  /+/ wrap,  prevAngle = a }
    
    where a    = bodyAngle b
          pos  = bodyPos b
          pos' = pos /+/ bodyVelocity b
          a'   = a + bodyRotation b
          wrap = wrapper pos'

-- | Generate body data is is between current and previous state.
interpolatedBody :: Float -- ^ interpolation point
                 -> Body  -- ^ body
                 -> Body  -- ^ interpolated body
interpolatedBody i b = b { bodyPos = pos', bodyAngle = a' }
    where pos' = (bodyPos b) /* i /+/ (prevPos b) /* i'
          a'   = (bodyAngle b) * i + (prevAngle b) * i'
          i'   = 1.0 - i
          
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
transform (Body pos a _ _ _ _) = applyXform $ (translatePt pos) . (rotatePt a)
