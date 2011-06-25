module Haskeroids.Geometry where

-- | Type alias for the value of vector components
type VecVal = Float

-- | Type alias for a 2D vector
type Vec2 = (VecVal, VecVal)

-- | Line segment between two points
newtype LineSegment = LineSegment (Vec2, Vec2)

-- | Conversion from polar to cartesian coordinates
polar :: VecVal -- ^ radial coordinate
      -> VecVal -- ^ anglular coordinate
      -> Vec2   -- ^ cartesian point
polar m a = (m * sin a, m * (-cos a))

-- | Transform a list of points into a list of connected line segments
pointsToSegments :: [Vec2] -> [LineSegment]
pointsToSegments (p:p':[])     = [LineSegment (p,p')]
pointsToSegments (p:t@(p':_)) = (LineSegment (p,p')) : pointsToSegments t

-- | Give a delta vector that needs to be added to point to wrap it around the
--   screen edge.
wrapper :: Vec2 -> Vec2
wrapper (x,y) = (x',y')
    where x' | x < 0 = 800
             | x >= 800 = -800
             | otherwise = 0
          y' | y < 0 = 600
             | y >= 600 = -600
             | otherwise = 0

             
ptDistanceSqr :: Vec2 -> Vec2 -> VecVal
ptDistanceSqr (x,y) (x',y') = dx*dx + dy*dy
    where dx = x-x'
          dy = y-y'

-- | Add or subtract two vectors
(x,y) /+/ (x',y') = (x+x', y+y')
(x,y) /-/ (x',y') = (x-x', y-y')

infixl 6 /+/
infixl 6 /-/

-- | Multiply a vector with a scalar
s */ (x,y) = (s*x, s*y)
(x,y) /* s = (s*x, s*y)

infixl 7 /*
infixl 7 */
