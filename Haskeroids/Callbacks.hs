module Haskeroids.Callbacks (
    renderViewport,
    handleKeyboard,
    logicTick) where

import Data.IORef
import Data.Time.Clock.POSIX

import Control.Monad (when)
import Control.Concurrent (threadDelay)

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard

type KeyboardRef = IORef Keyboard
type TimeRef = IORef POSIXTime

-- | Render the viewport using the given renderable and swap buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers

-- | Periodical logic tick
logicTick :: (LineRenderable t, Tickable t) => TimeRef -> KeyboardRef -> t -> IO ()
logicTick tr kb t = do
    prev <- readIORef tr
    current <- getPOSIXTime
    
    let delta = current - prev
    if (delta > 0.033) then do
        writeIORef tr current
        
        keys <- readIORef kb
        
        let newTickable = tick keys t
        displayCallback $= renderViewport newTickable
        addTimerCallback 0 $ logicTick tr kb newTickable
        postRedisplay Nothing
        
    else addTimerCallback 0 $ logicTick tr kb t

-- | Update the Keyboard state according to the event
handleKeyboard :: KeyboardRef -> KeyboardMouseCallback
handleKeyboard kb k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
