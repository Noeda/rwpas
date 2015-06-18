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
  , rollUniformR
  , distanceToPlayer
  , getDirectionTowardsPlayer
  , emitDecoration )
  where

import Control.Applicative
import Control.Lens hiding ( Level )
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Data
import Data.Foldable
import Data.Ord
import GHC.Generics
import RWPAS.CommonTypes
import RWPAS.Control
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
        EnterLevel (WorldCoordinates tgt new_lid) -> case w^.levelById new_lid of
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
  deriving ( Alternative, Functor, Applicative, Monad, Typeable, Generic
           , MonadState (AIControlState m a) )

-- | Returns an estimated distance to player.
distanceToPlayer :: (Monad m, IsAI a) => AIControlMonad m a Int
distanceToPlayer = do
  pos <- (^.position) <$> myActor
  AIControlMonad $ do
    w <- use aiWorld
    l <- use aiActorLevel
    return $ estimateDistance pos l (w^.currentActor) (w^.currentLevel) w

-- | Returns a guess on the direction where player might be.
--
-- Does not do path searching, simply points to the direction of the player.
-- This is also cheap.
getDirectionTowardsPlayer :: (Monad m, IsAI a) => AIControlMonad m a Direction8
getDirectionTowardsPlayer = do
  pos <- (^.position) <$> myActor
  lev <- myLevel
  AIControlMonad $ do
    w <- use aiWorld
    l <- use aiActorLevel

    let player_id = w^.currentActor
        level_id  = w^.currentLevel
    return $ fst $ minimumBy (comparing snd) $ flip fmap directions8 $ \dir -> (dir, case step dir pos lev of
      SameLevel new_pos -> estimateDistance new_pos l player_id level_id w
      EnterLevel (WorldCoordinates new_pos new_lvl_id) -> estimateDistance new_pos new_lvl_id player_id level_id w)

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

-- | Emits a decoration directly next to the actor to some direction.
--
-- Does not emit the decoration if the target position contains impassable
-- terrain (in that case this function is a no-op). Other actors don't block
-- decorations (but the decoration might not be seen under the actor).
emitDecoration :: Monad m => Direction8 -> Decoration -> AIControlMonad m a ()
emitDecoration dir decoration = do
  actor <- myActor
  lvl <- myLevel
  AIControlMonad $ do
    w <- use aiWorld
    lvl_id <- use aiActorLevel
    case step dir (actor^.position) lvl of
      SameLevel new_pos ->
        unless (impassable $ terrainFeature new_pos lvl) $
          aiWorld .= (w & levelById lvl_id._Just.decorationByCoordinate new_pos .~ decoration)
      EnterLevel (WorldCoordinates new_pos new_lvl_id) ->
        case w^.levelById new_lvl_id of
          Nothing -> empty
          Just new_lvl ->
            unless (impassable $ terrainFeature new_pos new_lvl) $
              aiWorld .= (w & levelById new_lvl_id._Just.decorationByCoordinate new_pos .~ decoration)

