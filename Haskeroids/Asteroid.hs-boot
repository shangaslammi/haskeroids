module Haskeroids.Asteroid
    ( Asteroid
    , RandomAsteroid
    ) where

import Haskeroids.Random
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

type Hitpoints = Int
data Size = Small|Medium|Large
data Asteroid = Asteroid
    { asteroidSize  :: Size
    , asteroidBody  :: Body
    , asteroidHits  :: Hitpoints
    , asteroidLines :: [LineSegment]
    }
type RandomAsteroid = Random Asteroid