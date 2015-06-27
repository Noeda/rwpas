{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.World
  ( module RWPAS.World.Type
  , Command(..)
  , cycleWorld
  , performCommand )
  where

import           Control.Applicative
import           Control.Lens hiding ( Level, levels )
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Data.Data
import           GHC.Generics
import           RWPAS.Actor
import           RWPAS.Control.ControlMonad
import           RWPAS.Direction
import           RWPAS.Level
import           RWPAS.World.Type
import           RWPAS.Role.Psychic
import           System.Random.MWC

data Command
  = Move !Direction8
  | ActivateAura
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

performCommand :: PrimMonad m
               => Command
               -> Gen (PrimState m)
               -> World
               -> m (Maybe World)
performCommand (Move dir) _ world = return $ flip execStateT world $ do
  let (lvl, lvl_id, _, actor_id) = currentLevelAndActor world
  case tryMoveActor actor_id dir lvl_id lvl (\lid -> world^.levelById lid) of
    Just (new_lvl, maybe_more_levels) -> do
      levelById lvl_id .= Just new_lvl
      case maybe_more_levels of
        Nothing -> return ()
        Just (more_level_id, more_level) ->
          levelById more_level_id .= Just more_level
    _ -> empty

performCommand ActivateAura rng world =
  let (_, level_id, actor, actor_id) = currentLevelAndActor world
      transition = runAIControlMonadWithoutAI (activate RepulsionForce (WorldCoordinates (actor^.position) level_id))
   in do new_world <- transition world actor_id level_id rng
         return (Just new_world)

cycleWorld :: PrimMonad m
           => Gen (PrimState m)
           -> World
           -> m World
cycleWorld rng = execStateT $ do
  w <- get
  iforOf_ eachLevel w $ \level_id _ -> do
    current_w <- get
    new_w <- lift $ cycleLevel level_id current_w rng
    put new_w

