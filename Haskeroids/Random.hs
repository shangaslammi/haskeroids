{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haskeroids.Random
    ( randomBracket
    , randomBetween
    , randomPair
    , randomAngle
    , randomElliptical
    , nrandomR
    ) where

import qualified System.Random as R
import Control.Monad (liftM2, replicateM)
import Control.Monad.State

newtype Random a = Random { runR :: State (R.StdGen) a} deriving Monad

randomBetween :: R.Random a => (a,a) -> Random a
randomBetween b = Random $ do
    g <- get
    let (a, g') = R.randomR b g
    put g'
    return a

randomBracket :: (Num a, R.Random a) => a -> Random a
randomBracket a = randomBetween (-a, a)

randomPair :: (Num a, R.Random a) => a -> Random (a,a)
randomPair a = liftM2 (,) (randomBracket a) (randomBracket a)

randomAngle :: (R.Random a, Floating a) => Random a
randomAngle = randomBetween (0, pi * 2.0)

randomElliptical :: (R.Random a, Floating a) => (a,a) -> (a,a) -> Random (a, a)
randomElliptical xb yb = do
    ang  <- randomAngle
    xrad <- randomBetween xb
    yrad <- randomBetween yb
    return (cos ang * xrad, sin ang * yrad)

nrandomR :: R.Random a => Int -> (a,a) -> Random [a]
nrandomR n = replicateM n . randomBetween
