module Haskeroids.Initialize where

import Data.IORef
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.State (initialGameState)
import Haskeroids.Callbacks
import Haskeroids.Keyboard (initKeyboard)

-- | Set up the main application window
initializeWindow = do
    _ <- getArgsAndInitialize
    initialWindowSize  $= Size 800 600
    initialDisplayMode $= [DoubleBuffered] 
    createWindow "Haskeroids"
    
-- | Set up the initial OpenGL parameters
initializeOpenGL = do
    -- Disable depth checking as we won't be needing it in 2D
    depthMask $= Disabled
    
    -- Nicer line drawing
    lineSmooth  $= Enabled
    blend       $= Enabled
    blendFunc   $= (SrcAlpha,OneMinusSrcAlpha)
    lineWidth   $= 2.0    
    
    -- Set up viewport
    viewport   $= (Position 0 0, Size 800 600)
    
    -- Set up an orthogonal projection for 2D rendering
    matrixMode $= Projection
    loadIdentity
    ortho 0 800 600 0 (-1) 1
    matrixMode $= Modelview 0
    loadIdentity
    
    -- Set background color to dark bluish black
    clearColor $= Color4 0.0 0.0 0.1 1.0

-- | Set up GLUT callbacks
initializeCallbacks = do
    kb <- newIORef initKeyboard
    tr <- getPOSIXTime >>= newIORef
    keyboardMouseCallback $= Just (handleKeyboard kb)
    displayCallback $= renderViewport initialGameState
    addTimerCallback 0 (logicTick tr kb initialGameState)
