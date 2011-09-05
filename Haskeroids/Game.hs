
module Haskeroids.Game where

import Haskeroids.Asteroid
import Haskeroids.Bullet
import Haskeroids.Events
import Haskeroids.Particles
import Haskeroids.Player
import Haskeroids.Random
import Haskeroids.Text.Font

data GameState = GameState
    { objects   :: GameObjects
    , particles :: ParticleSystem
    , gameUI    :: GameUI
    }

data GameObjects = GameObjects
    { player    :: Player
    , bullets   :: [Bullet]
    , asteroids :: [Asteroid]
    }

data GameResources = GameResources
    { gameFont :: Font
    }

data GameUI = GameUI

runEvents :: (GameState, RandomGen) -> [GameEvent] -> (GameState, RandomGen)
runEvents = foldr step where
    step ev (gs@(GameState go@(GameObjects pl bs as) ps ui),rng) = case ev of

        NewAsteroid ra  -> (gs {objects = go {asteroids = a:as}}, rng') where
            (a,rng') = runRandom ra rng

        NewParticles pg -> (gs {particles = ps'}, rng') where
            (ps',rng') = runRandom (initNewParticles new ps) rng
            new = snd $ runParticleGen pg

        NewBullet nb    -> (gs {objects = go {bullets = nb:bs}},rng)
