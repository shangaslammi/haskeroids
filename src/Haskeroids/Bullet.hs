
module Haskeroids.Bullet
    ( Bullet
    , initBullet
    , updateBullets
    ) where

import Haskeroids.Render
import Haskeroids.Collision
import Haskeroids.Particles
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

import Control.Applicative

-- | Speed of a bullet in pixels per tick
bulletSpeed :: Float
bulletSpeed = 10.0

-- | The visual line for a bullet
bulletLine :: LineSegment
bulletLine  = LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed/2.0))

-- | The collision line for a bullet
bulletLine' :: LineSegment
bulletLine' = LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed))

-- | The maximum number of ticks that a bullet stays active
bulletMaxLife :: Int
bulletMaxLife = 70

data Bullet = Bullet
    { bulletLife :: Int
    , bulletBody :: Body
    }

instance LineRenderable Bullet where
    interpolatedLines f (Bullet _ b) = [transform b' bulletLine] where
        b' = interpolatedBody f b

instance Collider Bullet where
    collisionCenter  = bodyPos . bulletBody
    collisionRadius  = const bulletSpeed
    collisionLines b = return $ transform (bulletBody b) bulletLine'

    collisionParticles b = do
        let body    = bulletBody b
            emitDir = bodyAngle body + pi

        addParticles 5 NewParticle
            { npPosition  = bodyPos body
            , npRadius    = 3
            , npDirection = emitDir
            , npSpread    = pi/2
            , npSpeed     = (2.0, 5.0)
            , npLifeTime  = (5, 15)
            , npSize      = (1,2)
            }

-- | Initialize a new bullet with the given position and direction
initBullet :: Vec2 -> Float -> Bullet
initBullet pos angle = Bullet bulletMaxLife body where
    body = Body pos' angle vel 0 pos' angle
    vel  = polar bulletSpeed angle
    pos' = pos /+/ polar 12.0 angle

-- | Update a bullet to a new position
updateBullet :: Bullet -> Bullet
updateBullet (Bullet l b) = Bullet (l-1) $ updateBody b

-- | Update a list of bullets
updateBullets :: [Bullet] -> [Bullet]
updateBullets = filter bulletActive . map updateBullet

-- | Test wether a bullet is still active
bulletActive :: Bullet -> Bool
bulletActive (Bullet l _) = l > 0
