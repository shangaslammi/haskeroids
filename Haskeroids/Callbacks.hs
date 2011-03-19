module Haskeroids.Callbacks (
    renderViewport,
    handleKeyboard,
    logicTick) where

import Data.IORef

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard

type KeyboardRef = IORef Keyboard

-- | Render the viewport using the given renderable and swap buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers

-- | Periodical logic tick
logicTick :: (LineRenderable t, Tickable t) => KeyboardRef -> t -> IO ()
logicTick kb t = do
    keys <- readIORef kb
    let newTickable = tick keys t
    displayCallback $= renderViewport newTickable
    addTimerCallback 33 $ logicTick kb newTickable
    postRedisplay Nothing

-- | Update the Keyboard state according to the event
handleKeyboard :: KeyboardRef -> KeyboardMouseCallback
handleKeyboard kb k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
