
module Haskeroids.Random
    ( randomBracket
    , randomBetween
    , randomPair
    , randomAngle
    , randomElliptical
    , nrandomR
    , initRandomGen
    , runRandom
    , Random
    , RandomGen
    ) where

import qualified System.Random as R
import Control.Monad (liftM2, replicateM, Monad)
import Control.Monad.State

type RandomGen = R.StdGen

newtype Random a = Random { runR :: State RandomGen a}

instance Functor Random where
  fmap f = Random . fmap f . runR

instance Applicative Random where
  pure = Random . pure
  (Random statef) <*> (Random statex) = Random (statef <*> statex)

instance Monad Random where
    (Random a) >>= b = Random $ a >>= (runR . b)

initRandomGen :: Int -> RandomGen
initRandomGen = R.mkStdGen

runRandom :: Random a -> RandomGen -> (a, RandomGen)
runRandom r = runState (runR r)

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
