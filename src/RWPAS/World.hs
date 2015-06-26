{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.World
  ( World()
  , HasWorld(..)
  , Command(..)
  , WorldCoordinates(..)
  , RunningID
  , runningID
  -- * Construction of worlds
  , singletonWorld
  -- * Messages
  , insertMessage
  , currentMessages
  , Message(..)
  -- * Accessing levels
  , levelById
  , terrainAt
  -- * Field of view
  , getCurrentFieldOfView
  -- * Simulating the world
  , cycleWorld
  , computeFieldOfView
  -- * Managing actors
  , relocateActor
  , actorAt
  , performCommand
  , currentLevelAndActor
  , actorNextToPlayer
  , estimateDistance )
  where

import           Control.Applicative
import           Control.Lens hiding ( Level, levels )
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Data
import           Data.Foldable
import           Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Sequence as SQ
import           Data.Text ( Text )
import           Data.Word
import           GHC.Generics hiding ( to )
import           Linear.V2
import           RWPAS.Actor
import           RWPAS.Direction
import           RWPAS.Level
import           RWPAS.TwoDimensionalVector
import           RWPAS.WorldCoordinates
import           System.Random.MWC

type FieldOfView = Vector2DG (Word8, Word8, Word8)

type RunningID = Int

-- | Type of messages. The integer in constructor is the number of times the
-- message has been repeated.
data Message
  = Message !Int !Text
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

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

data Command
  = Move !Direction8
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

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

getCurrentFieldOfView :: World -> (Int, Int, Int -> Int -> (Maybe ActorAppearance, Maybe TerrainFeature, Decoration))
getCurrentFieldOfView world =
  (51, 51, \x y ->
    let (avalue, tvalue, dvalue) = getAt (V2 x y) (world^.currentFieldOfView) (0, 0, 0)
     in (if avalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ avalue-1)
        ,if tvalue == 0
           then Nothing
           else Just $ toEnum (fromIntegral $ tvalue-1)
        ,toEnum (fromIntegral dvalue)))
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
    fov <- newMutable 51 51 (0, 0, 0)
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

        modify $ \old -> (coords, level_id, tfeature):old
        lift $ writeToMutable fov ox oy (actor_value, terrain_value, decoration_value)

    v <- unsafeFreezeVector2D fov
    return (v, memory_updates)


singletonWorld :: Level -> World
singletonWorld initial_level = computeFieldOfView World
  { _levels       = IM.singleton 0 (insertActor 1 firstActor initial_level)
  , _currentLevel = 0
  , _currentActor = 1
  , _currentFieldOfView = generate 51 51 $ \_ _ -> (0, 0, 0)
  , _runningID    = 2
  , _messages     = mempty }
 where
  firstActor = sentinelActor &
    (position .~ V2 250 250) .
    (actorHitPoints .~ Just (emptyHitPoints & (hp .~ 30) .
                                              (maxHp .~ 30)))

currentLevelAndActor :: World -> (Level, LevelID, Actor, ActorID)
currentLevelAndActor world =
  let level = fromMaybe (emptyLevel "Unnamed level") (world^.levels.at (world^.currentLevel))
      actor = fromMaybe sentinelActor (level^.actorById (world^.currentActor))
   in (level, world^.currentLevel, actor, world^.currentActor)
{-# INLINE currentLevelAndActor #-}

performCommand :: Command -> World -> Maybe World
performCommand (Move dir) world = flip execStateT world $ do
  lvl <- fromJust <$> use (levels.at (world^.currentLevel))
  case tryMoveActor (world^.currentActor) dir (world^.currentLevel) lvl (\lid -> world^.levels.at lid) of
    Just (new_lvl, maybe_more_levels) -> do
      levels.at (world^.currentLevel) .= Just new_lvl
      case maybe_more_levels of
        Nothing -> return ()
        Just (more_level_id, more_level) ->
          levels.at more_level_id .= Just more_level
    _ -> empty

cycleWorld :: PrimMonad m
           => Gen (PrimState m)
           -> World
           -> m World
cycleWorld rng = execStateT $ do
  w <- get
  ifor_ (w^.levels) $ \level_id _ -> do
    current_w <- get
    new_w <- lift $ cycleLevel level_id current_w rng
    put new_w

-- | Returns `True` if an actor on given level is next to a position.
actorNextToPlayer :: WorldCoordinates -> World -> Bool
actorNextToPlayer (WorldCoordinates coordinates level_id) world = fromMaybe False $ do
  level <- world^.levels.at level_id
  player_level <- world^.levels.at (world^.currentLevel)
  player_actor <- player_level^.actorById (world^.currentActor)

  let test level_id coords = level_id == world^.currentLevel &&
                             coords == player_actor^.position

  return $ flip any directions8 $ \dir -> case step dir coordinates level of
    SameLevel coords -> test level_id coords
    EnterLevel (WorldCoordinates coords new_level_id) -> test new_level_id coords

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

-- | Takes an actor and moves it to target.
--
-- Returns `Nothing` if the attempted move is illegal (e.g. actor doesn't exist
-- or there's an attempt to move it to non-existent level or if actor would
-- occupy the space of another actor).
relocateActor :: ActorID -> LevelID -> LevelID -> LevelCoordinates -> World -> Maybe World
relocateActor aid src_level_id tgt_level_id target_coordinates world = do
  src_level <- world^.levels.at src_level_id
  tgt_level <- world^.levels.at tgt_level_id
  actor <- src_level^.actorById aid
  let new_actor = actor & position .~ target_coordinates

  case actorByCoordinates target_coordinates tgt_level of
    Just tgt_aid | tgt_aid == aid -> return world
    Just _ -> Nothing
    Nothing -> do
      let new_src_level = removeActor aid src_level
          new_tgt_level = insertActor aid new_actor $
                          removeActor aid tgt_level

      -- Note: src_level and tgt_level may be the same and actually in most
      -- cases are. This is why target level is set last; it'll overwrite
      -- src_level if they are the same.
      return $ world &
        (levels.at tgt_level_id .~ Just new_tgt_level) .
        (levels.at src_level_id .~ Just new_src_level)
-- Inline because this is an often called function.
{-# INLINE relocateActor #-}

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

levelById :: LevelID -> Lens' World (Maybe Level)
levelById lid = levels.at lid

