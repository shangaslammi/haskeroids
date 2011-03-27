
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

initBullet :: Vec2 -> Float -> Bullet
initBullet pos angle = Bullet $ Body pos angle vel 0 (pos /-/ vel) angle
    where vel = polar bulletSpeed angle
    
updateBullet :: Bullet -> Bullet
updateBullet (Bullet b) = Bullet $ updateBody b