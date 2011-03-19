module Haskeroids.Tick where

import Haskeroids.Keyboard (Keyboard)
import Haskeroids.Geometry.Body (HasBody(..), updateBody)

class Tickable t where
    tick :: Keyboard -> t -> t
