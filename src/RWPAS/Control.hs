{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Control
  ( World()
  , Command(..)
  , performCommand
  , singletonWorld
  , levelById
  , currentActorLevelAndCoordinates
  , getCurrentFieldOfView )
  where

import           Control.Lens hiding ( Level, levels )
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Data
import           Data.Foldable
import           Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IM
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Word
import           GHC.Generics
import           Linear.V2
import           RWPAS.Actor
import           RWPAS.Direction
import           RWPAS.Level
import           RWPAS.TwoDimensionalVector

data Command
  = Move !Direction8
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type FieldOfView = Vector2DG (Word8, Word8)

data World = World
  { _levels             :: !(IntMap Level)
  , _currentLevel       :: !LevelID
  , _currentActor       :: !ActorID
  , _currentFieldOfView :: !FieldOfView
  , _runningID          :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''World

levelById :: LevelID -> Lens' World (Maybe Level)
levelById lid = levels.at lid

getCurrentFieldOfView :: World -> (Int, Int, Int -> Int -> (Maybe ActorAppearance, Maybe TerrainFeature))
getCurrentFieldOfView world =
  (51, 51, \x y ->
    let (avalue, tvalue) = getAt (V2 x y) (world^.currentFieldOfView) (0, 0)
     in (if avalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ avalue-1)
        ,if tvalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ tvalue-1)))
{-# INLINE getCurrentFieldOfView #-}

computeFieldOfView :: World -> World
computeFieldOfView world =
  case world^?levels.at (world^.currentLevel)._Just.actorById actor_id of
    Just (Just actor) ->
      let (fov, memory_updates) = actually_compute_fov actor
       in world & (currentFieldOfView .~ fov) .
                  layout_memory_updates memory_updates
    _ -> world
 where
  actor_id = world^.currentActor

  layout_memory_updates [] world = world
  layout_memory_updates lst world =
    let map = foldl' (\map (coords, level_id, feature) ->
                       M.alter (\case
                         Nothing  -> Just $ M.singleton coords feature
                         Just map -> Just $ M.insert coords feature map)
                                level_id
                                map)
                     (M.empty :: Map LevelID (Map LevelCoordinates TerrainFeature))
                     lst
     in flip execState world $
          for_ (M.assocs map) $ \(level_id, updates) ->
            levels.at level_id %= fmap (updateActorMemories actor_id updates)

  actually_compute_fov actor = runST $ do
    fov <- newMutable 51 51 (0, 0)
    memory_updates <- flip execStateT [] $
      levelFieldOfView 23
                       23
                       (actor^.position)
                       (fromJust $ world^.levels.at (world^.currentLevel))
                       (world^.currentLevel)
                       (\lid -> world^.levels.at lid) $ \coords (V2 x y) lvl level_id -> do
        let ox = x + 25 - actor^.position._x
            oy = y + 25 - actor^.position._y
            tfeature = terrainFeature coords lvl
            terrain_value = 1 + fromIntegral (fromEnum tfeature)
            actor_value = fromMaybe 0 $ do
                            aid <- actorByCoordinates coords lvl
                            ac <- lvl^.actorById aid
                            return $ 1 + fromIntegral (fromEnum $ ac^.appearance)

        modify $ \old -> (coords, level_id, tfeature):old
        lift $ writeToMutable fov ox oy (actor_value, terrain_value)

    v <- unsafeFreezeVector2D fov
    return (v, memory_updates)


singletonWorld :: Level -> World
singletonWorld initial_level = computeFieldOfView World
  { _levels       = IM.singleton 0 (insertActor 1 (sentinelActor & position .~ V2 250 250) initial_level)
  , _currentLevel = 0
  , _currentActor = 1
  , _currentFieldOfView = generate 51 51 $ \_ _ -> (0, 0)
  , _runningID    = 2 }

currentActorLevelAndCoordinates :: World -> (Level, LevelID, Actor, ActorID)
currentActorLevelAndCoordinates world =
  let level = fromMaybe (emptyLevel "Unnamed level") (world^.levels.at (world^.currentLevel))
      actor = fromMaybe sentinelActor (level^.actorById (world^.currentActor))
   in (level, world^.currentLevel, actor, world^.currentActor)

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
      modify computeFieldOfView
    _ -> return ()

