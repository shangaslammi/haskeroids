
module Haskeroids.Render where

import Haskeroids.Util

class LineRenderable r where
    lineSegments :: r -> [LineSegment]