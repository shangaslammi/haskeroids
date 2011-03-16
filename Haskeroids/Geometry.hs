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
pointsToSegments (p:t@(p':ps)) = (LineSegment (p,p')) : pointsToSegments t

-- | Translate a point
translate :: Vec2 -- ^ (x,y) delta
          -> Vec2 -- ^ original point
          -> Vec2 -- ^ translated point
translate (x,y) (x',y') = (x+x', y+y')

-- | Translate a line segment
translateLine :: Vec2 -> LineSegment -> LineSegment
translateLine p (LineSegment (l,l')) = LineSegment (t l, t l')
    where t = translate p
    
