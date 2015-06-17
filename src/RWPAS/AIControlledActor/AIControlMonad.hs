{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module RWPAS.AIControlledActor.AIControlMonad
  ( myActor
  , myLevel
  , move
  , HasAIState(..)
  , AIControlMonad()
  , runAIControlMonad
  , rollUniform
  , rollUniformR )
  where

import Control.Applicative
import Control.Lens hiding ( Level )
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Data
import GHC.Generics
import RWPAS.CommonTypes
import RWPAS.Direction
import RWPAS.Level
import System.Random.MWC

data AIControlState m a = AIControlState
  { _aiWorld :: World
  , _aiRNG :: Gen (PrimState m)
  , _aiActor :: ActorID
  , _aiActorLevel :: LevelID
  , _aiActorState :: a }
  deriving ( Typeable, Generic, Functor, Foldable, Traversable )
makeLenses ''AIControlState

class HasAIState a b | a -> b where
  aiState :: Lens' a b

instance HasAIState (AIControlState m a) a where
  aiState = aiActorState

instance HasWorld (AIControlState m a) where
  world = aiWorld

-- | Access the current actor.
myActor :: Monad m => AIControlMonad m a Actor
myActor = AIControlMonad $ do
  my_actor_id <- use aiActor
  my_level_id <- use aiActorLevel
  world <- use aiWorld
  case world^.levelById my_level_id of
    Nothing -> empty
    Just lvl -> case lvl^.actorById my_actor_id of
      Nothing -> empty
      Just actor -> return actor

-- | Access the current level.
myLevel :: Monad m => AIControlMonad m a Level
myLevel = AIControlMonad $ do
  my_level_id <- use aiActorLevel
  world <- use aiWorld
  case world^.levelById my_level_id of
    Nothing -> empty
    Just lvl -> return lvl
{-# INLINE myLevel #-}

-- | Move actor to some direction.
--
-- Fails in the `Alternative` instance if actor cannot move there.
move :: Monad m => Direction8 -> AIControlMonad m a ()
move dir = AIControlMonad $ do
  aid <- use aiActor
  lid <- use aiActorLevel
  w <- use aiWorld
  case w^.levelById lid of
    Nothing -> empty
    Just level -> case level^.actorById aid of
      Nothing -> empty
      Just actor -> case step dir (actor^.position) level of
        SameLevel tgt -> do
          guardPassable level tgt
          aiWorld.levelById lid._Just %= removeActor aid
          aiWorld.levelById lid._Just %= insertActor aid (actor & position .~ tgt)
        EnterLevel new_lid tgt -> case w^.levelById new_lid of
          Nothing -> empty
          Just new_level -> do
            guardPassable new_level tgt
            aiActorLevel .= new_lid
            aiWorld.levelById lid._Just %= removeActor aid
            aiWorld.levelById new_lid._Just %= insertActor aid (actor & position .~ tgt)
 where
  guardPassable level target_coordinates =
    case actorByCoordinates target_coordinates level of
      Just _ -> empty
      Nothing ->
        when (impassable (terrainFeature target_coordinates level)) empty
{-# INLINE move #-}

-- | A monad for implementing artificial intelligences, with some utility
-- functions in it.
--
-- This is purely for convenience and AIs don't have to actually use it.
newtype AIControlMonad m a r = AIControlMonad (StateT (AIControlState m a) (MaybeT m) r)
  deriving ( Alternative, Functor, Applicative, Monad, Typeable, Generic )

runAIControlMonad :: (Monad m, IsAI a) => AIControlMonad m a () -> AITransition m a
runAIControlMonad (AIControlMonad monad) actor_state rng world actor_id level_id = do
  let maybe = execStateT monad AIControlState
                { _aiWorld = world
                , _aiRNG = rng
                , _aiActorState = actor_state
                , _aiActorLevel = level_id
                , _aiActor = actor_id }
  result <- runMaybeT maybe
  return $ case result of
    Nothing -> world
    Just ai_control_state ->
      ai_control_state^.aiWorld &
       levelById level_id._Just.actorAIs.at (ai_control_state^.aiActor) .~ Just (AI $ ai_control_state^.aiActorState)

rollUniform :: (PrimMonad m, Variate v) => AIControlMonad m a v
rollUniform = AIControlMonad $ do
  rng <- use aiRNG
  lift $ lift $ uniform rng
{-# INLINE rollUniform #-}

rollUniformR :: (PrimMonad m, Variate v) => (v, v) -> AIControlMonad m a v
rollUniformR range = AIControlMonad $ do
  rng <- use aiRNG
  lift $ lift $ uniformR range rng
{-# INLINE rollUniformR #-}

