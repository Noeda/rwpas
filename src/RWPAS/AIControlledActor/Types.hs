{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module RWPAS.AIControlledActor.Types
  ( SentinelAI()
  , AI(..)
  , AITransition
  , IsAI(..)
  -- * Stepping AI
  , stepAI
  -- * AI implementation utilities
  , AIControlMonad()
  , runAIControlMonad
  , myActor
  , myLevel
  -- ** Using the random number generator
  , rollUniform
  , rollUniformR )
  where

import Control.Applicative
import Control.Lens hiding ( Level )
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Data
import Data.SafeCopy
import GHC.Generics
import RWPAS.Actor
import RWPAS.CommonTypes
import System.Random.MWC

-- | Sessile AI, used as a placeholder.
data SentinelAI = SentinelAI
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
deriveSafeCopy 0 'base ''SentinelAI

stepAI :: MonadIO m => AI -> GenIO -> World -> ActorID -> LevelID -> m (AI, World)
stepAI (AI state) rng world actor_id level_id =
  liftIO (transitionFunction state rng world actor_id level_id >>= \(new_state, world) ->
          return (AI new_state, world))
{-# INLINE stepAI #-}

data AIControlState a = AIControlState
  { _aiWorld :: World
  , _aiRNG :: GenIO
  , _aiActor :: ActorID
  , _aiActorLevel :: LevelID
  , _aiActorState :: a }
  deriving ( Typeable, Generic, Functor, Foldable, Traversable )
makeLenses ''AIControlState

-- | A monad for implementing artificial intelligences, with some utility
-- functions in it.
--
-- This is purely for convenience and AIs don't have to actually use it.
newtype AIControlMonad a r = AIControlMonad (StateT (AIControlState a) (MaybeT IO) r)
  deriving ( Alternative, Functor, Applicative, Monad, Typeable, Generic )

runAIControlMonad :: AIControlMonad a () -> AITransition a
runAIControlMonad (AIControlMonad monad) actor_state rng world actor_id level_id = do
  let maybe = execStateT monad AIControlState
                { _aiWorld = world
                , _aiRNG = rng
                , _aiActorState = actor_state
                , _aiActorLevel = level_id
                , _aiActor = actor_id }
  result <- runMaybeT maybe
  return $ case result of
    Nothing -> (actor_state, world)
    Just ai_control_state ->
      (ai_control_state^.aiActorState, ai_control_state^.aiWorld)

rollUniform :: Variate v => AIControlMonad a v
rollUniform = AIControlMonad $ do
  rng <- use aiRNG
  liftIO $ uniform rng
{-# INLINE rollUniform #-}

rollUniformR :: Variate v => (v, v) -> AIControlMonad a v
rollUniformR range = AIControlMonad $ do
  rng <- use aiRNG
  liftIO $ uniformR range rng
{-# INLINE rollUniformR #-}

-- | Access the current actor.
myActor :: AIControlMonad a Actor
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
myLevel :: AIControlMonad a Level
myLevel = AIControlMonad $ do
  my_level_id <- use aiActorLevel
  world <- use aiWorld
  case world^.levelById my_level_id of
    Nothing -> empty
    Just lvl -> return lvl
{-# INLINE myLevel #-}

class HasAIState a b | a -> b where
  aiState :: Lens' a b

instance HasAIState (AIControlState a) a where
  aiState = aiActorState

instance HasWorld (AIControlState a) where
  world = aiWorld

