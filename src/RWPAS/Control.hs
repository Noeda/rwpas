{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Control
  ( World()
  , Command(..)
  , performCommand
  , singletonWorld
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
  = Move !Direction
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type LevelID = Int

data World = World
  { _levels       :: !(IntMap Level)
  , _currentLevel :: !LevelID
  , _currentActor :: !ActorID
  , _runningID    :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''World

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
  case tryMoveActor (world^.currentActor) dir lvl of
    Just new_lvl -> levels.at (world^.currentLevel) .= Just new_lvl
    _ -> return ()

