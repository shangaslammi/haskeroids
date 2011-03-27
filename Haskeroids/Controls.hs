module Haskeroids.Controls (
    turnRight,
    turnLeft,
    thrust,
    shoot,
    ) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

turnRight = SpecialKey KeyRight
turnLeft  = SpecialKey KeyLeft
thrust    = SpecialKey KeyUp
shoot     = Char ' '
