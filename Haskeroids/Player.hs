module Haskeroids.Player (
    Player(..),
    initPlayer,
    collidePlayer) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard (isKeyDown)
import Haskeroids.Controls
import Haskeroids.Asteroid
import Haskeroids.Collision
import Haskeroids.Bullet

-- | Data type for tracking current player state
data Player = Player {
    playerBody   :: Body,
    playerAlive  :: Bool,
    playerBullet :: Maybe Bullet
    }

instance LineRenderable Player where
    interpolatedLines _ (Player _ False _) = []
    interpolatedLines f (Player b _ _) = map (transform b') $ shipLines
        where b' = interpolatedBody f b

instance Tickable Player where
    tick _  p@(Player _ False _) = p
    tick kb p@(Player b _ _) = p {
            playerBody   = updatePlayerBody turn acc b,
            playerBullet = bullet }
        where turn | key turnLeft  = -0.18
                   | key turnRight = 0.18
                   | otherwise     = 0
              
              acc | key thrust = 0.7
                  | otherwise  = 0
                  
              bullet | key shoot = Just $ initBullet (bodyPos b) (bodyAngle b)
                     | otherwise = Nothing
                  
              key = isKeyDown kb
              
instance Collider Player where
    collisionCenter = bodyPos . playerBody
    collisionRadius = const shipSize
    collisionLines  = interpolatedLines 0

-- | Test collision between the player ship and a list of Colliders
--   If the ship intersects with any, it is destroyed
collidePlayer :: Collider a => Player -> [a] -> Player
collidePlayer p@(Player _ False _) _ = p
collidePlayer p [] = p
collidePlayer p a = p { playerAlive = not $ any (collides p) a }

-- | Initial state for the player ship at center of the screen
initPlayer :: Player
initPlayer = Player (initBody (400,300) 0) True Nothing

-- | Update the player ship with the given turn rate and acceleration
updatePlayerBody :: Float -> Float -> Body -> Body
updatePlayerBody turn acc = updateBody . damping 0.96 . accForward acc . rotate turn
    
-- | Constant for the ship size
shipSize = 12.0 :: Float

-- | List of lines that make up the ship hull
shipLines :: [LineSegment]
shipLines = pointsToSegments shipPoints

shipPoints :: [Vec2]
shipPoints = [polar shipSize      0,
              polar shipSize      (0.7*pi),
              polar (shipSize*0.2) pi,
              polar shipSize      (1.3*pi),
              polar shipSize      0]
