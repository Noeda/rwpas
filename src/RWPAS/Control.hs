{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Control
  ( World()
  , Command(..)
  , performCommand
  , singletonWorld
  , levelById
  , currentActorLevelAndCoordinates )
  where

import           Control.Lens hiding ( Level, levels )
import           Control.Monad.State.Strict
import           Data.Data
import           Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           GHC.Generics
import           RWPAS.Actor
import           RWPAS.Direction
import           RWPAS.Level

data Command
  = Move !Direction4
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data World = World
  { _levels       :: !(IntMap Level)
  , _currentLevel :: !LevelID
  , _currentActor :: !ActorID
  , _runningID    :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''World

levelById :: LevelID -> Lens' World (Maybe Level)
levelById lid = levels.at lid

singletonWorld :: Level -> World
singletonWorld initial_level = World
  { _levels       = IM.singleton 0 (insertActor 1 sentinelActor initial_level)
  , _currentLevel = 0
  , _currentActor = 1
  , _runningID    = 2 }

currentActorLevelAndCoordinates :: World -> (Level, Actor)
currentActorLevelAndCoordinates world =
  let level = fromMaybe emptyLevel (world^.levels.at (world^.currentLevel))
      actor = fromMaybe sentinelActor (level^.actorById (world^.currentActor))
   in (level, actor)

performCommand :: Command -> World -> World
performCommand (Move dir) world = flip execState world $ do
  lvl <- fromJust <$> use (levels.at (world^.currentLevel))
  case tryMoveActor (world^.currentActor) dir (world^.currentLevel) lvl (\lid -> world^.levels.at lid) of
    Just (new_lvl, maybe_more_levels) -> do
      levels.at (world^.currentLevel) .= Just new_lvl
      case maybe_more_levels of
        Nothing -> return ()
        Just (more_level_id, more_level) ->
          levels.at more_level_id .= Just more_level
    _ -> return ()

