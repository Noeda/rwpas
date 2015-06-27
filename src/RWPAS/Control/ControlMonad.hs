{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Control.ControlMonad
  ( myActor
  , myActorID
  , myLevel
  , myCoordinates
  , move
  , moveCoords
  , emitMessage
  , withNChance
  , HasAIState(..)
  , ControlMonad
  , ControlActorMonad
  , AIControlMonad()
  , runAIControlMonad
  , runAIControlMonadWithoutAI
  , runWorldControlMonad
  , rollUniform
  , rollUniformR
  , distanceToPlayer
  , getDirectionTowardsPlayer
  , emitDecoration
  , leaveCorpse )
  where

import Control.Applicative
import Control.Lens hiding ( Level )
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Ord
import Data.Text ( Text )
import GHC.Generics
import RWPAS.Actor
import RWPAS.Control
import RWPAS.Direction
import RWPAS.Item
import RWPAS.Level.Type
import RWPAS.RNG
import RWPAS.WorldCoordinates
import RWPAS.World.Type as W
import System.Random.MWC

data AIControlState m a = AIControlState
  { _aiWorld :: World
  , _aiRNG :: Gen (PrimState m)
  , _aiActor :: ActorID
  , _aiActorLevel :: LevelID
  , _aiActorState :: a }
  deriving ( Typeable, Generic, Functor, Foldable, Traversable )
makeLenses ''AIControlState

data WorldControlState m = WorldControlState
  { _wWorld :: World
  , _wRNG :: Gen (PrimState m) }
  deriving ( Typeable, Generic )
makeLenses ''WorldControlState

class HasAIState a b | a -> b where
  aiState :: Lens' a b

instance HasAIState (AIControlState m a) a where
  aiState = aiActorState

instance HasWorld (AIControlState m a) where
  world = aiWorld

instance HasWorld (WorldControlState m) where
  world = wWorld

instance HasControllingActor (AIControlState m a) where
  controllingLevel = aiActorLevel
  controllingActor = aiActor

instance PrimMonad m => HasRNG (AIControlMonad m a) where
  rollUniform = AIControlMonad $ do
    rng <- use aiRNG
    lift $ uniform rng

  rollUniformR range = AIControlMonad $ do
    rng <- use aiRNG
    lift $ uniformR range rng

instance PrimMonad m => HasRNG (WorldControlMonad m) where
  rollUniform = WorldControlMonad $ do
    rng <- use wRNG
    lift $ uniform rng

  rollUniformR range = WorldControlMonad $ do
    rng <- use wRNG
    lift $ uniformR range rng

myCoordinates :: ControlActorMonad s m => m WorldCoordinates
myCoordinates = do
  actor <- myActor
  my_level_id <- use controllingLevel
  return $ WorldCoordinates (actor^.position) my_level_id
{-# INLINE myCoordinates #-}

-- | Access the current actor.
myActor :: ControlActorMonad s m => m Actor
myActor = do
  my_actor_id <- use controllingActor
  my_level_id <- use controllingLevel
  world <- use world
  case world^.levelById my_level_id of
    Nothing -> empty
    Just lvl -> case lvl^.actorById my_actor_id of
      Nothing -> empty
      Just actor -> return actor
{-# INLINE myActor #-}

class HasControllingActor s where
  controllingActor :: Lens' s ActorID
  controllingLevel :: Lens' s LevelID

myActorID :: ControlActorMonad s m => m ActorID
myActorID = use controllingActor
{-# INLINE myActorID #-}

-- | Access the current level.
myLevel :: ControlActorMonad s m => m Level
myLevel = do
  my_level_id <- use controllingLevel
  world <- use world
  case world^.levelById my_level_id of
    Nothing -> empty
    Just lvl -> return lvl
{-# INLINE myLevel #-}

-- | Move world coordinates to some direction.
--
-- Fails in the `Alternative` instance if the coordinates or destination is
-- invalid.
moveCoords :: ControlMonad s m => Direction8 -> WorldCoordinates -> m WorldCoordinates
moveCoords dir (WorldCoordinates coords lvl_id) = do
  w <- use world
  case w^.levelById lvl_id of
    Nothing -> empty
    Just lvl -> return $ case step dir coords lvl of
      SameLevel new_coords -> WorldCoordinates new_coords lvl_id
      EnterLevel new_coords -> new_coords
{-# INLINE moveCoords #-}

-- | Move actor to some direction.
--
-- Fails in the `Alternative` instance if actor cannot move there.
move :: ControlActorMonad s m => Direction8 -> m ()
move dir = do
  aid <- use controllingActor
  lid <- use controllingLevel
  w <- use world
  case w^.levelById lid of
    Nothing -> empty
    Just level -> case level^.actorById aid of
      Nothing -> empty
      Just actor -> case step dir (actor^.position) level of
        SameLevel tgt -> do
          guardPassable level tgt
          world.levelById lid._Just %= removeActor aid
          world.levelById lid._Just %= insertActor aid (actor & position .~ tgt)
        EnterLevel (WorldCoordinates tgt new_lid) -> case w^.levelById new_lid of
          Nothing -> empty
          Just new_level -> do
            guardPassable new_level tgt
            controllingLevel .= new_lid
            world.levelById lid._Just %= removeActor aid
            world.levelById new_lid._Just %= insertActor aid (actor & position .~ tgt)
 where
  guardPassable level target_coordinates =
    case actorByCoordinates target_coordinates level of
      Just _ -> empty
      Nothing ->
        when (impassable (terrainFeature target_coordinates level)) empty
{-# INLINE move #-}

-- | Values of type `AIControlMonad` describe how to modify the world with some
-- actor being a focus. Or you can say it's an artificial intelligence monad
-- for actors.
newtype AIControlMonad m a r = AIControlMonad (StateT (AIControlState m a) (MaybeT m) r)
  deriving ( Alternative, Functor, Applicative, Monad, Typeable, Generic
           , MonadState (AIControlState m a) )

-- | A monad for implementing effects on world. Very much like `AIControlMonad`
-- but has no focus on any particular actor.
newtype WorldControlMonad m r = WorldControlMonad (StateT (WorldControlState m) (MaybeT m) r)
  deriving ( Alternative, Functor, Applicative, Monad, Typeable, Generic
           , MonadState (WorldControlState m) )

-- | Returns an estimated distance to player.
distanceToPlayer :: ControlActorMonad s m => m Int
distanceToPlayer = do
  pos <- (^.position) <$> myActor
  w <- use world
  l <- use controllingLevel
  let (_, current_level, _, current_actor) = currentLevelAndActor w
  return $ estimateDistance pos l current_actor current_level w

-- | Returns a guess on the direction where player might be.
--
-- Does not do path searching, simply points to the direction of the player.
-- This is also cheap.
getDirectionTowardsPlayer :: ControlActorMonad s m => m Direction8
getDirectionTowardsPlayer = do
  pos <- (^.position) <$> myActor
  lev <- myLevel
  w <- use world
  l <- use controllingLevel

  let (_, level_id, _, player_id) = currentLevelAndActor w
  return $ fst $ minimumBy (comparing snd) $ flip fmap directions8 $ \dir -> (dir, case step dir pos lev of
    SameLevel new_pos -> estimateDistance new_pos l player_id level_id w
    EnterLevel (WorldCoordinates new_pos new_lvl_id) -> estimateDistance new_pos new_lvl_id player_id level_id w)

runWorldControlMonad :: Monad m
                     => WorldControlMonad m ()
                     -> World
                     -> Gen (PrimState m)
                     -> m World
runWorldControlMonad (WorldControlMonad monad) initial_world rng = do
  let maybe = execStateT monad WorldControlState { _wWorld = initial_world
                                                 , _wRNG   = rng }
  runMaybeT maybe >>= return . \case
    Nothing -> initial_world
    Just new_state -> new_state^.world
{-# INLINE runWorldControlMonad #-}

-- | Runs an `AIControlMonad` but without an AI.
runAIControlMonadWithoutAI :: Monad m
                           => (forall a. AIControlMonad m a ())
                           -> World
                           -> ActorID
                           -> LevelID
                           -> Gen (PrimState m)
                           -> m World
runAIControlMonadWithoutAI (AIControlMonad monad) initial_world actor_id level_id rng = do
  let maybe = execStateT monad AIControlState
                { _aiWorld = initial_world
                , _aiRNG = rng
                , _aiActorState = ()
                , _aiActorLevel = level_id
                , _aiActor = actor_id }
  runMaybeT maybe >>= return . \case
    Nothing -> initial_world
    Just new_world -> new_world^.world

-- | Runs an `AIControlMonad` and updates actor AI after running it.
runAIControlMonad :: (IsAI a, Monad m) => AIControlMonad m a () -> AITransition m a
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
       levelById level_id._Just.actorById (ai_control_state^.aiActor)._Just.ai .~ (AI $ ai_control_state^.aiActorState)

type ControlMonad s m = (Alternative m, MonadState s m, HasWorld s, HasRNG m)

type ControlActorMonad s m = (Alternative m, MonadState s m, HasWorld s, HasControllingActor s, HasRNG m)

-- | Emits a decoration directly next to the actor to some direction.
--
-- Does not emit the decoration if the target position contains impassable
-- terrain (in that case this function is a no-op). Other actors don't block
-- decorations (but the decoration might not be seen under the actor).
emitDecoration :: ControlActorMonad s m => Direction8 -> Decoration -> m ()
emitDecoration dir decoration = do
  actor <- myActor
  lvl <- myLevel
  w <- use world
  lvl_id <- use controllingLevel
  case step dir (actor^.position) lvl of
    SameLevel new_pos ->
      unless (impassable $ terrainFeature new_pos lvl) $
        world .= (w & levelById lvl_id._Just.decorationByCoordinate new_pos .~ decoration)
    EnterLevel (WorldCoordinates new_pos new_lvl_id) ->
      case w^.levelById new_lvl_id of
        Nothing -> empty
        Just new_lvl ->
          unless (impassable $ terrainFeature new_pos new_lvl) $
            world .= (w & levelById new_lvl_id._Just.decorationByCoordinate new_pos .~ decoration)

emitMessage :: (MonadState s m, HasWorld s)
            => Text -> m ()
emitMessage txt = world %= insertMessage txt

-- | With a chance in 1/N, execute the action.
withNChance :: ControlMonad s m => Int -> m () -> m ()
withNChance n action = do
  x <- rollUniformR (1, n)
  when (x == 1) action

-- | Leaves a corpse of some appearance and removes the actor.
--
-- Utility function that is useful in dead transition.
leaveCorpse :: ControlActorMonad s m => ItemAppearance -> Text -> m ()
leaveCorpse appearance corpse_name = do
  coords <- myCoordinates
  ac <- myActor
  world.itemAt coords .= Just (sentinelItem (corpse_name <> " corpse") &
                               itemAppearance .~ appearance)
  world.actorAt coords .= Nothing
  emitMessage $ ac^.actorName <> " is DEAD, R.I.P."

