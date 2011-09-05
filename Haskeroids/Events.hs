
module Haskeroids.Events
    ( GameEvent(..)
    , newAsteroid
    , newParticles
    , newBullet
    , runGame
    ) where

import Control.Monad

import Haskeroids.Bullet
import Haskeroids.Asteroid
import Haskeroids.Particles

data GameEvent
    = NewAsteroid RandomAsteroid
    | NewParticles (ParticleGen ())
    | NewBullet Bullet

data GameMonad v = Return v | EmitEvent GameEvent (GameMonad v)

instance Monad GameMonad where
    return          = Return
    (Return x)      >>= f = f x
    (EmitEvent e m) >>= f = EmitEvent e (m >>= f)

emit :: GameEvent -> GameMonad ()
emit ev = EmitEvent ev (Return ())

newAsteroid :: RandomAsteroid -> GameMonad ()
newAsteroid = emit . NewAsteroid

newParticles :: ParticleGen () -> GameMonad ()
newParticles = emit . NewParticles

newBullet :: Bullet -> GameMonad ()
newBullet = emit . NewBullet

runGame :: GameMonad v -> (v, [GameEvent])
runGame (Return x) = (x, [])
runGame (EmitEvent e m) = (x, e:es) where
    (x,es) = runGame m
