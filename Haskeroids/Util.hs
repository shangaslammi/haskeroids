module Haskeroids.Util where

-- | Type alias for the value of point components
type PtVal = Float

-- | Type alias for a 2D point
type Pt2 = (PtVal, PtVal)

-- | Line segment between two points
newtype LineSegment = LineSegment (Pt2, Pt2)

-- | Conversion from radial to cartesian coordinates
radial :: Float -- ^ magnitude
       -> Float -- ^ angle
       -> Pt2   -- ^ cartesian point
radial m a = (m * sin a, m * (-cos a))

-- | Transform a list of points into a list of connected line segments
linesToSegments :: [Pt2] -> [LineSegment]
linesToSegments (p:p':[])     = [LineSegment (p,p')]
linesToSegments (p:t@(p':ps)) = (LineSegment (p,p')) : linesToSegments t

-- | Translate a point
ptTranslate :: Pt2 -- ^ (x,y) delta
            -> Pt2 -- ^ original point
            -> Pt2 -- ^ translated point
ptTranslate (x,y) (x',y') = (x+x', y+y')

-- | Translate a line segment
lsTranslate :: Pt2 -> LineSegment -> LineSegment
lsTranslate p (LineSegment (l,l')) = LineSegment (t l, t l')
    where t = ptTranslate p
    