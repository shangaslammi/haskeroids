module Haskeroids.Player
    ( Player(..)
    , initPlayer
    , collidePlayer
    , tickPlayer
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard (Keyboard, isKeyDown)
import Haskeroids.Controls
import Haskeroids.Asteroid
import Haskeroids.Collision
import Haskeroids.Bullet
import Haskeroids.Particles

import Data.Maybe (isJust)
import Control.Arrow ((***))

-- | Data type for tracking current player state
data Player = Player
    { playerBody   :: Body
    , playerAlive  :: Bool
    , playerBullet :: Maybe Bullet
    , playerROF    :: Int
    }

-- | Constant for the ship size
shipSize :: Float
shipSize = 12.0

-- | Constant for the delay (in ticks) between firing bullets
fireDelay :: Int
fireDelay = 5

-- | Damping factor for ship velocity
shipDamping :: Float
shipDamping = 0.96

instance LineRenderable Player where
    interpolatedLines f (Player body alive _ _)
        | not alive = []
        | otherwise = map (transform b') shipLines where
            b' = interpolatedBody f body

instance Collider Player where
    collisionCenter = bodyPos . playerBody
    collisionRadius = const shipSize
    collisionLines  = interpolatedLines 0

-- | Handle keyboard input and update the player ship
tickPlayer :: Keyboard -> Player -> ParticleGen Player
tickPlayer kb p@(Player body alive _ rof)
    | not alive = return $ p { playerBullet = Nothing }
    | otherwise = return $ Player body' True bullet rof' where

        body'   = updatePlayerBody turn acc body

        turn
            | key turnLeft  = -0.18
            | key turnRight =  0.18
            | otherwise     =  0

        acc
            | key thrust = 0.7
            | otherwise  = 0

        bullet
            | rof == 0 && key shoot = Just newBullet
            | otherwise             = Nothing

        rof'
            | isJust bullet = fireDelay
            | otherwise     = if rof > 0 then rof - 1 else 0

        newBullet = initBullet (bodyPos body) (bodyAngle body)
        key       = isKeyDown kb

-- | Test collision between the player ship and a list of Colliders
--   If the ship intersects with any, it is destroyed
collidePlayer :: Collider a => [a] -> Player -> ParticleGen Player
collidePlayer _  p@(Player _ False _ _) = return p
collidePlayer [] p = return p
collidePlayer a  p = return $ p { playerAlive = not $ any (collides p) a }

-- | Initial state for the player ship at center of the screen
initPlayer :: Player
initPlayer = Player (initBody (400,300) 0 (0,0) 0) True Nothing 0

-- | Update the player ship with the given turn rate and acceleration
updatePlayerBody :: Float -> Float -> Body -> Body
updatePlayerBody turn acc =
    updateBody . damping shipDamping . accForward acc . rotate turn

-- | List of lines that make up the ship hull
shipLines :: [LineSegment]
shipLines = pointsToSegments shipPoints

shipPoints :: [Vec2]
shipPoints = map (uncurry polar . multiply) points where
    multiply = (shipSize*) *** (pi*)
    points   =
       [ (1.0, 0.0)
       , (1.0, 0.7)
       , (0.2, 1.0)
       , (1.0, 1.3)
       , (1.0, 0.0)
       ]
