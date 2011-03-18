module Haskeroids.Keyboard (
    Keyboard,
    initKeyboard,
    handleKeyEvent,
    isKeyDown,
    SpecialKey(..)) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT

newtype Keyboard = Keyboard (Set SpecialKey)

initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent (SpecialKey k) ks (Keyboard s) = case ks of
    Up   -> Keyboard $ Set.delete k s
    Down -> Keyboard $ Set.insert k s
handleKeyEvent _ _ kb = kb

isKeyDown :: Keyboard -> SpecialKey -> Bool
isKeyDown (Keyboard s) k = Set.member k s