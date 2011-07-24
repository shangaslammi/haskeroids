module Haskeroids.Collision (Collider(..)) where

import Control.Applicative

import Haskeroids.Geometry

class Collider c where
    -- | Line segments used for collision detection
    collisionLines :: c -> [LineSegment]

    -- | Center and radius of a bounding circle
    collisionCenter :: c -> Vec2
    collisionRadius :: c -> Float

    -- | Test if two colliders intersect
    collides :: (Collider d) => c -> d -> Bool
    collides c c' = canCollide && doesCollide where
        canCollide  = distSqr < radius*radius
        doesCollide = or $ lineCollision <$> cl <*> cl'

        distSqr = ptDistanceSqr (collisionCenter c) (collisionCenter c')
        radius  = collisionRadius c + collisionRadius c'

        cl  = collisionLines c
        cl' = collisionLines c'

-- | Test if two line segments intersect
lineCollision :: LineSegment -> LineSegment -> Bool
lineCollision (LineSegment ((x1,y1),(x2,y2))) (LineSegment ((x3,y3),(x4,y4)))
    | d == 0    = False
    | otherwise = ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1 where
        ua = na/d
        ub = nb/d
        d  = (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
        na = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
        nb = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
