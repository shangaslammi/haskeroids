module Haskeroids.Callbacks
    ( initCallbackRefs
    , renderViewport
    , handleKeyboard
    ) where

import Data.IORef
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard
import Haskeroids.State

type KeyboardRef = IORef Keyboard
type TimeRef     = IORef POSIXTime
type StateRef    = IORef GameState
type AccumRef    = TimeRef
type PrevTimeRef = TimeRef

type CallbackRefs = (AccumRef, PrevTimeRef, KeyboardRef, StateRef)

secPerFrame :: Fractional a => a
secPerFrame = 0.0333

maxFrameTime :: Fractional a => a
maxFrameTime = 0.1

-- | Initialize a new group of callback references
initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
    accum <- newIORef secPerFrame
    prev  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    st    <- newIORef initialGameState
    return (accum, prev, keyb, st)

-- | Run the game logic, render the view and swap display buffers
renderViewport :: CallbackRefs -> IO ()
renderViewport (ar, pr, kb, sr) = do
    current <- getPOSIXTime
    accum   <- readIORef ar
    prev    <- readIORef pr
    keys    <- readIORef kb

    let consumeAccum acc s
            | acc >= secPerFrame =
                consumeAccum (acc - secPerFrame) $ tickState keys s
            | otherwise = (acc, s)

        frameTime = min (current - prev) maxFrameTime

    (accum', s') <- fmap (consumeAccum (accum + frameTime)) $ readIORef sr

    writeIORef sr s'
    writeIORef ar accum'
    writeIORef pr current

    clear [ColorBuffer]

    let interpolation = realToFrac $ accum' / secPerFrame
    renderInterpolated interpolation s'

    swapBuffers
    postRedisplay Nothing

-- | Update the Keyboard state according to the event
handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (_, _, kb, _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
