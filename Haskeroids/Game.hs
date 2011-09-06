{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}
module Haskeroids.Game where

import Haskeroids.Player
import Haskeroids.Bullet
import Haskeroids.Asteroid
import Haskeroids.Particles
import Haskeroids.Random
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard (Keyboard)
import Haskeroids.Text
import Haskeroids.Text.Font

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List (partition)

data Game = Game
    { player    :: Player
    , asteroids :: [Asteroid]
    , bullets   :: [Bullet]
    , particles :: ParticleSystem
    , texts     :: [Text]
    , randomGen :: RandomGen
    , font      :: Font
    } | NewGame Font

instance LineRenderable Game where
    interpolatedLines f (Game p a b s t _ _)
        = pls ++ als ++ bls ++ sls ++ tls where
        pls = interpolatedLines f p
        als = concatMap (interpolatedLines f) a
        bls = concatMap (interpolatedLines f) b
        sls = interpolatedLines f s
        tls = concatMap (interpolatedLines f) t

-- | Generate the initial game state
initialGameState :: Font -> Game
initialGameState = NewGame

-- | Tick state into a new game state
tickState :: Keyboard -> Game -> Game
tickState _ (NewGame f) = Game
    { player    = initPlayer
    , asteroids = a
    , bullets   = []
    , particles = initParticleSystem
    , texts     = []
    , randomGen = g
    , font      = f
    } where
    (a, g) = runRandom (replicateM 3 genInitialAsteroid) $ initRandomGen 0

tickState kb gs = tickGame gs where
    tickGame = execState $ do
        act $ tickPlayer kb
        ask playerBullet >>= possibly add

        alive <- ask playerAlive
        act' updateAsteroids >>= act . collidePlayer
        alive' <- ask playerAlive

        when (alive && not alive') $ do
            font <- getData
            add $ setTextCenter (400,300) $ mkText font 50 "game over"

        act $ collideAsteroids . updateBullets

        (remaining, destroyed) <- ask $ partition asteroidAlive
        putData remaining

        let newAsteroids = fmap concat $ mapM spawnNewAsteroids destroyed
        resolve newAsteroids >>= resolve . sequence >>= addAll

        act tickParticles

type GameState = State Game

class GameData a where
    getData :: GameState a
    putData :: a -> GameState ()

    modifyData :: (a -> a) -> GameState ()
    modifyData f = fmap f getData >>= putData

class (GameData [a]) => ListData a where
    add :: a -> GameState ()
    add a = modifyData (a:)

instance GameData a => GameData (Maybe a) where
    getData = fmap Just getData
    putData Nothing  = return ()
    putData (Just x) = putData x

instance GameData Player where
    getData = fmap player get
    putData p = modify $ \g -> g {player = p}

instance GameData [Asteroid] where
    getData = fmap asteroids get
    putData as = modify $ \g -> g {asteroids = as}

instance GameData [Bullet] where
    getData = fmap bullets get
    putData bs = modify $ \g -> g {bullets = bs}

instance GameData ParticleSystem where
    getData = fmap particles get
    putData ps = modify $ \g -> g {particles = ps}

instance GameData RandomGen where
    getData = fmap randomGen get
    putData rg = modify $ \g -> g {randomGen = rg}

instance GameData [Text] where
    getData = fmap texts get
    putData ts = modify $ \g -> g {texts = ts}

instance GameData Font where
    getData = fmap font get
    putData f = modify $ \g -> g {font = f}

instance ListData Asteroid where
instance ListData Bullet where
instance ListData Text where

instance (GameData a, GameData b) => GameData (a,b) where
    getData = (,) <$> getData <*> getData
    putData (a,b) = putData a >> putData b

instance (GameData a, GameData b, GameData c) => GameData (a,b,c) where
    getData = (,,) <$> getData <*> getData <*> getData
    putData (a,b,c) = putData a >> putData b >> putData c

class GameAction a where
    act :: a -> GameState ()

instance (GameData a) => GameAction (a -> a) where
    act = modifyData

instance (GameData a, GameData b) => GameAction (a -> b -> (a,b)) where
    act f = (f <$> getData <*> getData) >>= putData

instance (GameData a, GameAction b) => GameAction (a -> b) where
    act f = (f <$> getData) >>= act

instance (GameData a) => GameAction (Random a) where
    act r = do
        rng <- getData
        putData $ runRandom r rng

instance (GameData a) => GameAction (ParticleGen a) where
    act pg = do
        let (a,np) = runParticleGen pg
        putData a
        act $ initNewParticles np

class Wrapped a where
    resolve :: a b -> GameState b

instance Wrapped ParticleGen where
    resolve pg = do
        let (a,np) = runParticleGen pg
        act $ initNewParticles np
        return a

instance Wrapped Random where
    resolve r = do
        rng <- getData
        let (a,rng') = runRandom r rng
        putData rng'
        return a

act' :: GameData a => (a -> a) -> GameState a
act' f = do
    a <- fmap f getData
    putData a
    return a

ask :: GameData a => (a -> b) -> GameState b
ask f = fmap f getData

addAll :: (GameData [a]) => [a] -> GameState ()
addAll as = modifyData (as++)

possibly :: Applicative f => (a -> f ()) -> (Maybe a) -> f ()
possibly _ Nothing  = pure ()
possibly f (Just x) = f x
