module Haskeroids.Random
    ( randomBracket
    , randomPair
    , randomAngle
    , randomElliptical
    , nrandomR
    ) where

import System.Random (Random, randomRIO)
import Control.Monad (liftM2, replicateM)

randomBracket :: (Num a, Random a) => a -> IO a
randomBracket a = randomRIO (-a, a)

randomPair :: (Num a, Random a) => a -> IO (a,a)
randomPair a = liftM2 (,) (randomBracket a) (randomBracket a)

randomAngle :: (Random a, Floating a) => IO a
randomAngle = randomRIO (0, pi * 2.0)

randomElliptical :: (Random a, Floating a) => (a,a) -> (a,a) -> IO (a, a)
randomElliptical xb yb = do
    ang  <- randomAngle
    xrad <- randomRIO xb
    yrad <- randomRIO yb
    return (cos ang * xrad, sin ang * yrad)

nrandomR :: Random a => Int -> (a,a) -> IO [a]
nrandomR n = replicateM n . randomRIO
