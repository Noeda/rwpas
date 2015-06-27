{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module RWPAS.World.Type
  (
  -- * Types
    World()
  , HasWorld(..)
  , singletonWorld
  -- * Running IDs
  , RunningID
  , runningID
  -- * Messages
  , Message(..)
  , insertMessage
  , currentMessages
  -- * Field of view
  , getCurrentFieldOfView
  , computeFieldOfView
  -- * Actor access
  , currentLevelAndActor
  , actorAt
  -- * Level access
  , terrainAt
  , itemAt
  , levelById
  , eachLevel
  -- * Distances
  , estimateDistance
  , module RWPAS.WorldCoordinates )
  where

import Control.Lens hiding ( Level, levels )
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Data
import Data.Foldable
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Sequence as SQ
import Data.Text ( Text )
import Data.Word
import GHC.Generics hiding ( to )
import Linear.V2
import RWPAS.Actor
import RWPAS.Item
import RWPAS.Level.Type
import RWPAS.TwoDimensionalVector
import RWPAS.WorldCoordinates

type FieldOfView = Vector2DG (Word8, Word8, Word8, Word8)

-- | Type of messages. The integer in constructor is the number of times the
-- message has been repeated.
data Message
  = Message !Int !Text
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type RunningID = Int

data World = World
  { _levels             :: !(IntMap Level)
  , _currentLevel       :: !LevelID
  , _currentActor       :: !ActorID
  , _currentFieldOfView :: !FieldOfView
  , _runningID          :: !RunningID
  , _messages           :: !(SQ.Seq Message) }
  deriving ( Eq, Ord, Show, Typeable, Generic )
makeLenses ''World

class HasWorld a where
  world :: Lens' a World

instance HasWorld World where
  world = lens id (\_ new -> new)

insertMessage :: Text -> World -> World
insertMessage txt world =
  if SQ.length (new_world^.messages) > 100
    then new_world & messages .~ SQ.take 100 (new_world^.messages)
    else new_world
 where
  new_world = world & messages .~
    case SQ.viewl (world^.messages) of
      SQ.EmptyL ->
        SQ.singleton (Message 1 txt)
      Message n old_txt SQ.:< rest
        | old_txt == txt -> Message (n+1) old_txt SQ.<| rest
        | otherwise      -> Message 1 txt SQ.<| world^.messages

-- | Returns most recent messages, ordered by time. The most recent message is
-- first.
currentMessages :: World -> [Message]
currentMessages world = toList (world^.messages)

currentLevelAndActor :: World -> (Level, LevelID, Actor, ActorID)
currentLevelAndActor world =
  let level = fromMaybe (emptyLevel "Unnamed level") (world^.levels.at (world^.currentLevel))
      actor = fromMaybe (sentinelActor "Player") (level^.actorById (world^.currentActor))
   in (level, world^.currentLevel, actor, world^.currentActor)
{-# INLINE currentLevelAndActor #-}

-- | Lens to actor ID and actor at some coordinates.
--
-- Note: the level to which world coordinates point to must exist. If you try
-- to set an actor to invalid coordinates, this lens will throw an exception.
actorAt :: WorldCoordinates -> Lens' World (Maybe (ActorID, Actor))
actorAt (WorldCoordinates coords level_id) = lens get_it set_it
 where
  get_it w = do
    lvl <- w^.levelById level_id
    aid <- actorByCoordinates coords lvl
    ac <- lvl^.actorById aid
    return (aid, ac)

  set_it w Nothing = fromMaybe w $ do
    lvl <- w^.levelById level_id
    aid <- actorByCoordinates coords lvl
    return $ w & levelById level_id .~ Just (removeActor aid lvl)

  set_it w (Just (aid, ac)) =
    case w^.levelById level_id of
      Nothing -> error "set_it.actorAt: non-existent level."
      Just lvl -> w & levelById level_id .~ Just (insertActor aid (ac & position .~ coords) lvl)
{-# INLINE actorAt #-}

itemAt :: WorldCoordinates -> Lens' World (Maybe Item)
itemAt (WorldCoordinates coords level_id) = lens get_it set_it
 where
  get_it w = do
    lvl <- w^.levelById level_id
    lvl^.itemByCoordinates coords

  set_it w Nothing = w & levelById level_id %~ fmap (itemByCoordinates coords .~ Nothing)
  set_it w (Just item) = case w^.levelById level_id of
    Nothing -> error "set_it.itemAt: non-existent level."
    Just lvl -> w & levelById level_id .~ Just (lvl & itemByCoordinates coords .~ Just item)


-- | Getter to some coordinates in world.
--
-- Setting terrain would be so inefficient with a naive lens so we don't have
-- that here.
--
-- Getter instead of a plain function to be consistent with other accessing
-- functions.
terrainAt :: WorldCoordinates -> Getter World (Maybe TerrainFeature)
terrainAt (WorldCoordinates coords level_id) = to $ \w ->
  case w^.levelById level_id of
    Nothing -> Nothing
    Just lvl -> Just $ terrainFeature coords lvl

levelById :: LevelID -> Lens' World (Maybe Level)
levelById lid = levels.at lid

-- | Returns an estimation of distance to another actor, by diagonal distance.
--
-- May (grossly) overestimate the distance but never underestimates.
estimateDistance :: LevelCoordinates -> LevelID -> ActorID -> LevelID -> World -> Int
estimateDistance coords lvl1 aid2 lvl2 world
  | isNothing ac2' = maxBound
  | otherwise =
      -- if actors are on the same level, take their diagonal distance
      -- directly.
      if lvl1 == lvl2
        then diagonalDistance coords (ac2^.position)
        else maxBound
 where
  ac2' = world^.levelById lvl2 >>= \l -> l^.actorById aid2
  Just ac2 = ac2'
{-# INLINE estimateDistance #-}

getCurrentFieldOfView :: World -> (Int, Int, Int -> Int -> (Maybe ActorAppearance, Maybe TerrainFeature, Decoration, Maybe ItemAppearance))
getCurrentFieldOfView world =
  (51, 51, \x y ->
    let (avalue, tvalue, dvalue, ivalue) = getAt (V2 x y) (world^.currentFieldOfView) (0, 0, 0, 0)
     in (if avalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ avalue-1)
        ,if tvalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ tvalue-1)
        ,toEnum (fromIntegral dvalue)
        ,if ivalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ ivalue-1)))
{-# INLINE getCurrentFieldOfView #-}

-- | Updates the current field of view for current actor.
--
-- Call whenever the world is changed and you need to call
-- `getCurrentFieldOfView`. The computation is expensive so it is not done
-- automatically.
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
    fov <- newMutable 51 51 (0, 0, 0, 0)
    memory_updates <- flip execStateT [] $
      levelFieldOfView 23
                       23
                       (actor^.position)
                       (fromJust $ world^.levels.at (world^.currentLevel))
                       (world^.currentLevel)
                       (\lid -> world^.levels.at lid) $ \coords (V2 x y) lvl level_id -> do
        let ox = x + 25 - actor^.position._x
            oy = y + 25 - actor^.position._y
            decoration_value = fromIntegral $ fromEnum $ lvl^.decorationByCoordinate coords
            tfeature = terrainFeature coords lvl
            terrain_value = 1 + fromIntegral (fromEnum tfeature)
            actor_value = fromMaybe 0 $ do
                            aid <- actorByCoordinates coords lvl
                            ac <- lvl^.actorById aid
                            return $ 1 + fromIntegral (fromEnum $ ac^.appearance)
            item_value = fromMaybe 0 $ do
                           item <- lvl^.itemByCoordinates coords
                           return $ 1 + fromIntegral (fromEnum $ item^.itemAppearance)

        modify $ \old -> (coords, level_id, tfeature):old
        lift $ writeToMutable fov ox oy (actor_value, terrain_value, decoration_value, item_value)

    v <- unsafeFreezeVector2D fov
    return (v, memory_updates)

singletonWorld :: Level -> World
singletonWorld initial_level = computeFieldOfView World
  { _levels       = IM.singleton 0 (insertActor 1 firstActor initial_level)
  , _currentLevel = 0
  , _currentActor = 1
  , _currentFieldOfView = generate 51 51 $ \_ _ -> (0, 0, 0, 0)
  , _runningID    = 2
  , _messages     = mempty }
 where
  firstActor = sentinelActor "Player" &
    (position .~ V2 250 250) .
    (actorHitPoints .~ Just (emptyHitPoints & (hp .~ 30) .
                                              (maxHp .~ 30)))

-- | Indexed fold to levels.
eachLevel :: IndexedFold LevelID World Level
eachLevel = levels.ifolded

