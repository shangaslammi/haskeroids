module Haskeroids.Keyboard (
    Keyboard,
    initKeyboard,
    handleKeyEvent,
    isKeyDown,
    SpecialKey(..)) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT

-- | Set of all special keys that are currently held down
newtype Keyboard = Keyboard (Set SpecialKey)

-- | Create a new Keyboard manager
initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

-- | Record a key state change in the given Keyboard manager
handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent (SpecialKey k) ks (Keyboard s) = case ks of
    Up   -> Keyboard $ Set.delete k s
    Down -> Keyboard $ Set.insert k s
handleKeyEvent _ _ kb = kb

-- | Test if a key is currently held down in the given Keyboard
isKeyDown :: Keyboard -> SpecialKey -> Bool
isKeyDown (Keyboard s) k = Set.member k s
