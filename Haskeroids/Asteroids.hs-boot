module Haskeroids.Asteroid
    ( Asteroid
    , RandomAsteroid
    ) where

import Haskeroids.Geometry

type Hitpoints = Int
data Size = Small|Medium|Large deriving (Ord, Eq, Enum)
data Asteroid = Asteroid
    { asteroidSize  :: Size
    , asteroidBody  :: Body
    , asteroidHits  :: Hitpoints
    , asteroidLines :: [LineSegment]
    }
