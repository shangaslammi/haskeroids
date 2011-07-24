module Haskeroids.Particles
    ( ParticleSystem
    , initParticleSystem
    ) where

import Haskeroids.Render
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

particleLine :: LineSegment
particleLine = LineSegment ((0,1),(0,-1))

data Particle = Particle
    { particleBody :: Body
    , particleLife :: Int
    }

newtype ParticleSystem = ParticleSystem [Particle]

instance LineRenderable ParticleSystem where
    interpolatedLines f (ParticleSystem ps) = map (interpolateParticle f) ps

interpolateParticle :: Float -> Particle -> LineSegment
interpolateParticle f (Particle b _) = transform b' particleLine where
    b' = interpolatedBody f b

initParticleSystem :: ParticleSystem
initParticleSystem = ParticleSystem []
