module Haskeroids.Callbacks (
    initCallbackRefs,
    renderViewport,
    handleKeyboard) where

import Data.IORef
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard
import Haskeroids.State (GameState, initialGameState)

type KeyboardRef = IORef Keyboard
type TimeRef     = IORef POSIXTime
type StateRef    = IORef GameState

data CallbackRefs = CallbackRefs TimeRef TimeRef KeyboardRef StateRef

-- | Initialize a new group of callback references
initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
    accum <- newIORef $ 0.0333
    prev  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    st    <- newIORef initialGameState
    return $ CallbackRefs accum prev keyb st

-- | Render the viewport using the given renderable and swap buffers
renderViewport :: CallbackRefs -> IO ()
renderViewport refs@(CallbackRefs ar tr kb rr) = do
    current <- getPOSIXTime
    prev <- readIORef tr
    accum <- readIORef ar
    keys <- readIORef kb
    
    let frameTime = min 0.1 $ current - prev
        newAccum  = accum + frameTime

    let consumeAccum acc = if acc >= 0.0333
            then do
               modifyIORef rr $ tick keys
               consumeAccum $ acc - 0.0333
            else return acc
    
    newAccum' <- consumeAccum newAccum
    
    writeIORef tr current
    writeIORef ar newAccum'
    
    let interpolation = realToFrac $ newAccum' / 0.0333
    
    r <- readIORef rr
    
    clear [ColorBuffer]
    renderInterpolated interpolation r
    swapBuffers
    postRedisplay Nothing

-- | Update the Keyboard state according to the event
handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (CallbackRefs _ _ kb _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
