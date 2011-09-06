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
import Data.List (partition)

import qualified Control.Monad.State as S

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
    tickGame = S.execState $ do
        runUpdate $ tickPlayer kb
        ask playerBullet >>= possibly add

        update asteroids
        update bullets

        alive <- ask playerAlive

        collideAsteroids `with` bullets
        collidePlayer `with` asteroids

        alive' <- ask playerAlive

        when (alive && not alive') $ addText "game over" 50 (400,300)

        (remaining, destroyed) <- ask $ partition asteroidAlive
        put remaining

        newAsteroids <- run $ mapM splitAsteroid destroyed
        forM newAsteroids $ run >=> addAll

        update particles

    addText txt size pos = do
        font <- get
        add $ setTextCenter pos $ mkText font size txt

type GameState = S.State Game

class GameData a where
    get :: GameState a
    put :: a -> GameState ()

    modify :: (a -> a) -> GameState ()
    modify f = fmap f get >>= put

class (GameData [a]) => ListData a where
    add :: a -> GameState ()
    add a = modify (a:)

instance GameData a => GameData (Maybe a) where
    get = fmap Just get
    put Nothing  = return ()
    put (Just x) = put x

instance GameData Player where
    get = fmap player S.get
    put p = S.modify $ \g -> g {player = p}

instance GameData [Asteroid] where
    get = fmap asteroids S.get
    put as = S.modify $ \g -> g {asteroids = as}

instance GameData [Bullet] where
    get = fmap bullets S.get
    put bs = S.modify $ \g -> g {bullets = bs}

instance GameData ParticleSystem where
    get = fmap particles S.get
    put ps = S.modify $ \g -> g {particles = ps}

instance GameData RandomGen where
    get = fmap randomGen S.get
    put rg = S.modify $ \g -> g {randomGen = rg}

instance GameData [Text] where
    get = fmap texts S.get
    put ts = S.modify $ \g -> g {texts = ts}

instance GameData Font where
    get = fmap font S.get
    put f = S.modify $ \g -> g {font = f}

instance ListData Asteroid where
instance ListData Bullet where
instance ListData Text where

instance (GameData a, GameData b) => GameData (a,b) where
    get = (,) <$> get <*> get
    put (a,b) = put a >> put b

instance (GameData a, GameData b, GameData c) => GameData (a,b,c) where
    get = (,,) <$> get <*> get <*> get
    put (a,b,c) = put a >> put b >> put c

class GameData a => Updateable a where
    updateFunc :: (a -> a)

    update :: (Game -> a) -> GameState ()
    update f = fmap f S.get >>= put . updateFunc

instance Updateable [Asteroid] where
    updateFunc = tickAsteroids

instance Updateable [Bullet] where
    updateFunc = tickBullets

instance Updateable ParticleSystem where
    updateFunc = tickParticles

class Runnable a where
    run :: a b -> GameState b

instance Runnable ParticleGen where
    run pg = do
        let (a,np) = runParticleGen pg
        runUpdate $ initNewParticles np
        return a

instance Runnable Random where
    run r = do
        rng <- get
        let (a,rng') = runRandom r rng
        put rng'
        return a

class GameAction a where
    act :: a -> GameState ()

instance (GameData a) => GameAction (a -> a) where
    act = modify

instance (GameData a, GameData b) => GameAction (a -> b -> (a,b)) where
    act f = (f <$> get <*> get) >>= put

runUpdate :: (Runnable r, GameData a, GameData b) => (a -> r b) -> GameState ()
runUpdate f = f <$> get >>= run >>= put

with :: (Runnable r, GameData a, GameData c) =>
    (b -> a -> r c) -> (Game -> b) -> GameState ()
with f b = fmap f (S.gets b) >>= runUpdate

ask :: GameData a => (a -> b) -> GameState b
ask f = fmap f get

addAll :: (GameData [a]) => [a] -> GameState ()
addAll as = modify (as++)

possibly :: Applicative f => (a -> f ()) -> (Maybe a) -> f ()
possibly _ Nothing  = pure ()
possibly f (Just x) = f x
