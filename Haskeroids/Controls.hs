module Haskeroids.Controls (
    turnRight,
    turnLeft,
    ) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

turnRight = SpecialKey KeyRight
turnLeft  = SpecialKey KeyLeft
