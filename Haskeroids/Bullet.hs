
module Haskeroids.Bullet (
    Bullet,
    initBullet,
    updateBullet,
    bulletActive,
    collideBullets ) where

import Haskeroids.Render
import Haskeroids.Collision
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

import Control.Applicative

bulletSpeed = 10.0
bulletLine  = LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed/2.0))
bulletLine' = LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed))
bulletMaxLife = 70

data Bullet = Bullet {
    bulletLife :: Int,
    bulletBody :: Body }

instance LineRenderable Bullet where
    interpolatedLines f (Bullet _ b) = [transform b' bulletLine]
        where b' = interpolatedBody f b

instance Collider Bullet where
    collisionCenter = bodyPos . bulletBody
    collisionRadius = const bulletSpeed

    collisionLines b = return $ transform (bulletBody b) bulletLine'


-- | Initialize a new bullet with the given position and direction
initBullet :: Vec2 -> Float -> Bullet
initBullet pos angle = Bullet bulletMaxLife body
    where body = Body pos' angle vel 0 pos' angle
          vel  = polar bulletSpeed angle
          pos' = pos /+/ (polar 12.0 angle)

-- | Update a bullet to a new position
updateBullet :: Bullet -> Bullet
updateBullet (Bullet l b) = Bullet (l-1) $ updateBody b

-- | Collide bullets with other colliders
collideBullets :: Collider a => [a] -> [Bullet] -> [Bullet]
collideBullets c = filter (not.doesCollide)
    where doesCollide = or . (collides <$> c <*>) . return

-- | Test wether a bullet is still active
bulletActive (Bullet l _) = l > 0