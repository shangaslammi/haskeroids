
module Haskeroids.Events
    ( GameEvent(..)
    , Events
    , newAsteroid
    , newParticles
    , newBullet
    , runEvents
    ) where

import Control.Monad

import {-# SOURCE #-} Haskeroids.Bullet
import {-# SOURCE #-} Haskeroids.Asteroid
import Haskeroids.Particles

data GameEvent
    = NewAsteroid RandomAsteroid
    | NewParticles (ParticleGen ())
    | NewBullet Bullet

data Events v = Return v | EmitEvent GameEvent (Events v)

instance Monad Events where
    return          = Return
    (Return x)      >>= f = f x
    (EmitEvent e m) >>= f = EmitEvent e (m >>= f)

emit :: GameEvent -> Events ()
emit ev = EmitEvent ev (Return ())

newAsteroid :: RandomAsteroid -> Events ()
newAsteroid = emit . NewAsteroid

newParticles :: Int -> NewParticle -> Events ()
newParticles n = emit . NewParticles . addParticles n

newBullet :: Bullet -> Events ()
newBullet = emit . NewBullet

runEvents :: Events v -> (v, [GameEvent])
runEvents (Return x) = (x, [])
runEvents (EmitEvent e m) = (x, e:es) where
    (x,es) = runEvents m
