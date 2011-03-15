module Haskeroids.Util where

type PtVal = Float
type Pt2 = (PtVal, PtVal)
newtype LineSegment = LineSegment (Pt2, Pt2)

radial :: Float -- ^ magnitude
       -> Float -- ^ angle
       -> Pt2   -- ^ cartesian point
radial m a = (m * sin a, m * (-cos a))

linesToSegments :: [Pt2] -> [LineSegment]
linesToSegments (p:p':[])     = [LineSegment (p,p')]
linesToSegments (p:t@(p':ps)) = (LineSegment (p,p')) : linesToSegments t

ptTranslate :: Pt2 -> Pt2 -> Pt2
ptTranslate (x,y) (x',y') = (x+x', y+y')

lsTranslate :: Pt2 -> LineSegment -> LineSegment
lsTranslate (x,y) (LineSegment ((x',y'),(x'',y''))) = 
    LineSegment ((x+x',y+y'),(x+x'',y+y''))