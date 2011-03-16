module Haskeroids.Callbacks (renderViewport, logicTick) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick

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