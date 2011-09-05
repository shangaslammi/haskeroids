
module Haskeroids.Bullet
    ( Bullet
    ) where

import Haskeroids.Geometry.Body

data Bullet = Bullet
    { bulletLife :: Int
    , bulletBody :: Body
    }
