
module Haskeroids.Bullet (Bullet, initBullet, updateBullet) where

import Haskeroids.Render
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

data Bullet = Bullet Body

bulletSpeed = 10.0
bulletLine = LineSegment ((0,0),(0,bulletSpeed))

instance LineRenderable Bullet where
    interpolatedLines f (Bullet b) = [transform b' bulletLine]
        where b' = interpolatedBody f b

-- | Initialize a new bullet with the given position and direction
initBullet :: Vec2 -> Float -> Bullet
initBullet pos angle = Bullet $ Body (pos' /+/ vel) angle vel 0 pos' angle
    where vel  = polar bulletSpeed angle
          pos' = pos /+/ (polar 12.0 angle)
    
-- | Update a bullet to a new position
updateBullet :: Bullet -> Bullet
updateBullet (Bullet b) = Bullet $ updateBody b