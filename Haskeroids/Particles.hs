module Haskeroids.Particles
    ( ParticleSystem
    , NewParticle(..)
    , initParticleSystem
    ) where

import Haskeroids.Render
import Haskeroids.Random
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

particleLine :: LineSegment
particleLine = LineSegment ((0,1),(0,-1))

data Particle = Particle
    { particleBody :: Body
    , particleLife :: Int
    }

data NewParticle = NewParticle
    { position  :: Vec2
    , radius    :: Float
    , direction :: Float
    , spread    :: Float
    , speed     :: SpeedRange
    , lifeTime  :: LifeRange
    }

type RandomParticle = IO Particle
type Direction      = Float
type Spread         = Float
type SpeedRange     = (Float, Float)
type LifeRange      = (Int, Int)

newtype ParticleSystem = ParticleSystem [Particle]

instance LineRenderable ParticleSystem where
    interpolatedLines f (ParticleSystem ps) = map (interpolateParticle f) ps

interpolateParticle :: Float -> Particle -> LineSegment
interpolateParticle f (Particle b _) = transform b' particleLine where
    b' = interpolatedBody f b

initParticleSystem :: ParticleSystem
initParticleSystem = ParticleSystem []

initParticle :: NewParticle -> RandomParticle
initParticle (NewParticle p r d spr spd lt) = do
    e <- randomElliptical (0, r) (0, r)
    a <- randomBracket spr
    v <- randomBetween spd
    l <- randomBetween lt
    n <- randomAngle
    r <- randomBracket 10
    return Particle
        { particleBody = initBody (p /+/ e) n (polar v (a+d)) r
        , particleLife = l
        }

initNewParticles :: [NewParticle] -> ParticleSystem -> IO ParticleSystem
initNewParticles nps (ParticleSystem ps) = do
    p <- mapM initParticle nps
    return $ ParticleSystem $ p ++ ps
