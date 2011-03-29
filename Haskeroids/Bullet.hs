
module Haskeroids.Bullet (Bullet, initBullet, updateBullet) where

import Haskeroids.Render
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

bulletSpeed = 10.0
bulletLine = LineSegment ((0,0),(0,bulletSpeed))
bulletLife = 70

data Bullet = Bullet Int Body

instance LineRenderable Bullet where
    interpolatedLines f (Bullet _ b) = [transform b' bulletLine]
        where b' = interpolatedBody f b

-- | Initialize a new bullet with the given position and direction
initBullet :: Vec2 -> Float -> Bullet
initBullet pos angle = Bullet bulletLife $ Body (pos' /+/ vel) angle vel 0 pos' angle
    where vel  = polar bulletSpeed angle
          pos' = pos /+/ (polar 12.0 angle)
    
-- | Update a bullet to a new position
updateBullet :: Bullet -> Maybe Bullet
updateBullet (Bullet l b) = if l' > 0
        then Just $ Bullet l' $ updateBody b
        else Nothing
    where l' = l - 1
