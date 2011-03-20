module Haskeroids.Tick where

import Haskeroids.Keyboard (Keyboard)

class Tickable t where
    tick :: Keyboard -> t -> t
