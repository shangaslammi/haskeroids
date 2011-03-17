module Haskeroids.Callbacks (renderViewport, logicTick) where

import Data.IORef

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard


-- | Render the viewport using the given renderable and swap buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers

-- | Periodical logic tick
logicTick :: (LineRenderable t, Tickable t) => t -> IO ()
logicTick t = do
    let newTickable = tick t
    displayCallback $= renderViewport newTickable
    addTimerCallback 33 $ logicTick newTickable
    postRedisplay Nothing

-- | Update the Keyboard state according to the event
handleKeyboard :: IORef Keyboard -> Key -> KeyState -> Modifiers -> Position -> IO ()
handleKeyboard kb k ks _ _ = modifyIORef kb (handleKeyEvent k ks)