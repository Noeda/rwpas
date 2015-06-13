{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Level
  ( Level()
  , LevelID
  , TerrainFeature(..)
  , addPortal
  , insertActor
  , portalOnRightSideLevel
  , emptyLevel
  , roomLevel
  , terrainFeature
  , actors
  , actorById
  , actorByCoordinates
  , tryMoveActor
  , Size
  , LevelCoordinates
  , levelFieldOfView )
  where

import           Control.Lens hiding ( Level )
import           Control.Monad.State.Strict
import           Data.Data
import           Data.Foldable
import           Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import           Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import           Data.Maybe
import           GHC.Generics
import           Linear.V2
import           RWPAS.Actor
import           RWPAS.Direction
import           RWPAS.FieldOfView

data Level = Level
  { _terrain    :: !(Map LevelCoordinates TerrainFeature)
  , _portals    :: !(IntMap Portal)
  , _portalKeys :: !(Map LevelCoordinates IntSet)
  , _actorKeys  :: !(Map LevelCoordinates ActorID)
  , _actors     :: !(IntMap Actor) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Coordinates relative to some `Level`.
type LevelCoordinates = V2 Int

type LevelID = Int

-- | Describes the size of something.
type Size = V2 Int

data Portal = Portal
  { _axis                       :: !Direction4
  , _targetLevel                :: !LevelID
  , _targetLevelAxisTopPosition :: !Int
  , _targetLevelAxisPosition    :: !Int
  , _portalLength               :: !Int
  , _axisTopPosition            :: !Int
  , _axisPosition               :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type PortalID = Int

data TerrainFeature
  = Floor
  | Wall
  | Rock   -- ^ Same as `Wall` but completely black.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- Derive lenses here
makeLenses ''Level
makeLenses ''Portal

-- | If there's no feature at some coordinate, what we should assume it is?
defaultTerrainFeature :: TerrainFeature
defaultTerrainFeature = Rock

{-

A portal example:

Portal (axis = DRight, portalLength = 3, axisPosition = 3, axisTopPosition = 2)

x is marked to portal map, y is marked if axis = DLeft. The portal is the line
between xs and ys.

  1234
  .... 1
  .xy. 2
  .xy. 3
  .xy. 4
  .... 5

-}

addPortal :: Portal -> PortalID -> Level -> Level
addPortal portal portal_id = execState $ do
  case portal^.axis of
    DRight -> for_ [0..portal^.portalLength-1] $ \offset ->
      set_key (portal^.axisPosition-1) (offset + portal^.axisTopPosition)
    DLeft -> for_ [0..portal^.portalLength-1] $ \offset ->
      set_key (portal^.axisPosition) (offset + portal^.axisTopPosition)
    DDown -> for_ [0..portal^.portalLength-1] $ \offset ->
      set_key (offset + portal^.axisTopPosition) (portal^.axisPosition-1)
    DUp -> for_ [0..portal^.portalLength-1] $ \offset ->
      set_key (offset + portal^.axisTopPosition) (portal^.axisPosition)
  portals.at portal_id .= Just portal
 where
  set_key x y =
    let pos = V2 x y
     in portalKeys.at pos %= Just . \case
          Nothing -> IS.singleton portal_id
          Just set -> IS.insert portal_id set

-- | A completely empty level.
emptyLevel :: Level
emptyLevel = Level { _terrain    = mempty
                   , _actors     = mempty
                   , _actorKeys  = mempty
                   , _portals    = mempty
                   , _portalKeys = mempty }

-- | Same as `roomLevel` but adds a portal to the right side of the room.
--
-- The portal leads to the left side of the room. Pass the same `LevelID` as
-- the level itself.
portalOnRightSideLevel :: Size -> PortalID -> PortalID -> LevelID -> Level
portalOnRightSideLevel sz@(V2 w h) pid pid2 lid =
  let initial_level = roomLevel sz
   in addPortal Portal { _axis = DLeft
                       , _targetLevel = lid
                       , _targetLevelAxisTopPosition = 0
                       , _targetLevelAxisPosition    = w
                       , _portalLength = h
                       , _axisTopPosition = 0
                       , _axisPosition = 0 }
                pid2 $
      addPortal Portal { _axis = DRight
                       , _targetLevel = lid
                       , _targetLevelAxisTopPosition = 0
                       , _targetLevelAxisPosition = 0
                       , _portalLength = h
                       , _axisTopPosition = 0
                       , _axisPosition = w }
                pid
                initial_level

-- | A level that just has a single rectangular room. The walkable area is
-- sized according to the given coordinates, with (0, 0) being the top-left
-- corner of the room.
roomLevel :: Size -> Level
roomLevel (V2 w h) = Level { _terrain    = makeOneRoom w h
                           , _actors     = mempty
                           , _actorKeys  = mempty
                           , _portals    = mempty
                           , _portalKeys = mempty }
 where
  makeOneRoom w h = M.union
    -- The floor on the insides
    (M.fromList [ (V2 x y, Floor) | x <- [0..w-1], y <- [0..h-1] ])
    -- The walls on the sides
    (M.union (M.union (M.fromList [ (V2 (-1) y, Wall) | y <- [-1..h]])
                      (M.fromList [ (V2 w y, Wall) | y <- [-1..h]]))
             (M.union (M.fromList [ (V2 x (-1), Wall) | x <- [-1..w]])
                      (M.fromList [ (V2 x h, Wall) | x <- [-1..w]])))

-- | Lens to a terrain feature at some location.
terrainFeature :: LevelCoordinates -> Lens' Level TerrainFeature
terrainFeature coordinates = lens get_it set_it
 where
  get_it level = fromMaybe defaultTerrainFeature $ level^.terrain.at coordinates

  set_it level f | f == defaultTerrainFeature =
    level & terrain.at coordinates .~ Nothing
                 | otherwise =
    level & terrain.at coordinates .~ Just f

-- | Lens to an actor using some actor ID.
actorById :: ActorID -> Lens' Level (Maybe Actor)
actorById aid = actors.at aid

actorByCoordinates :: LevelCoordinates -> Level -> Maybe ActorID
actorByCoordinates coords level = level^.actorKeys.at coords

impassable :: TerrainFeature -> Bool
impassable Floor = False
impassable _ = True

seeThrough :: TerrainFeature -> Bool
seeThrough Floor = True
seeThrough _ = False

insertActor :: ActorID -> Actor -> Level -> Level
insertActor aid actor =
  (actors.at aid .~ Just actor) .
  (actorKeys.at (actor^.position) .~ Just aid)

tryMoveActor :: ActorID -> Direction4 -> LevelID -> Level -> (LevelID -> Maybe Level) -> Maybe (Level, Maybe (LevelID, Level))
tryMoveActor aid dir source_level_id level get_level = do
  actor <- IM.lookup aid (level^.actors)
  let actor_pos = actor^.position

  case step (direction4To8 dir) actor_pos level of
    SameLevel new_actor_pos ->
      if impassable (level^.terrainFeature new_actor_pos)
        then Nothing
        else Just (level &
                   (actorKeys.at new_actor_pos .~ Just aid) .
                   (actors.at aid .~ Just (actor & position .~ new_actor_pos)) .
                   (actorKeys.at actor_pos .~ Nothing), Nothing)
    EnterLevel new_level_id new_actor_pos ->
      -- TODO: check any complications if new level is the same as old one
      -- (that is, portal goes to level itself)
      --
      -- Right now it should be safe because RWPAS.Control sets the latter
      -- level last, overwriting the operation of removing the actor from the
      -- level.
      case get_level new_level_id of
        Nothing        -> Nothing
        Just new_level -> if impassable (new_level^.terrainFeature new_actor_pos)
            then Nothing
            else Just (level & (actors.at aid .~ Nothing) .
                               (actorKeys.at actor_pos .~ Nothing)
                      ,Just (new_level_id
                            ,new_level &
                             (actors.at aid .~ Just (actor & position .~ new_actor_pos)) .
                             (actorKeys.at new_actor_pos .~ Just aid) .
                             (if source_level_id == new_level_id
                               then actorKeys.at actor_pos .~ Nothing
                               else id)))

data StepResult
  = SameLevel !LevelCoordinates
  | EnterLevel !LevelID !LevelCoordinates
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

couldPotentiallyGoThroughPortal :: Direction8 -> Direction4 -> Bool
couldPotentiallyGoThroughPortal D8Up DUp = True
couldPotentiallyGoThroughPortal D8UpLeft DUp = True
couldPotentiallyGoThroughPortal D8UpRight DUp = True
couldPotentiallyGoThroughPortal _ DUp = False
couldPotentiallyGoThroughPortal D8Down DDown = True
couldPotentiallyGoThroughPortal D8DownLeft DDown = True
couldPotentiallyGoThroughPortal D8DownRight DDown = True
couldPotentiallyGoThroughPortal _ DDown = False
couldPotentiallyGoThroughPortal D8Left DLeft = True
couldPotentiallyGoThroughPortal D8DownLeft DLeft = True
couldPotentiallyGoThroughPortal D8UpLeft DLeft = True
couldPotentiallyGoThroughPortal _ DLeft = False
couldPotentiallyGoThroughPortal D8Right DRight = True
couldPotentiallyGoThroughPortal D8DownRight DRight = True
couldPotentiallyGoThroughPortal D8UpRight DRight = True
couldPotentiallyGoThroughPortal _ DRight = False

swapV2 :: V2 a -> V2 a
swapV2 (V2 x y) = V2 y x
{-# INLINE swapV2 #-}

step :: Direction8 -> LevelCoordinates -> Level -> StepResult
step dir coords@(V2 x y) level =
  case level^.portalKeys.at coords of
    Nothing -> SameLevel local_target
    Just set | IS.null set -> SameLevel local_target
    Just set ->
      case findInSet doesItGoThrough set of
        Nothing -> SameLevel local_target
        Just portal ->
          let initial_position_on_the_other_side =
                V2 (negate (portal^.axisPosition) + portal^.targetLevelAxisPosition)
                   (negate (portal^.axisTopPosition) + portal^.targetLevelAxisTopPosition)
              final_position_on_the_other_side =
                initial_position_on_the_other_side +
                direction8ToDelta dir

              fixed_position_on_the_other_side = case portal^.axis of
                DLeft  -> final_position_on_the_other_side + V2 x y
                DRight -> final_position_on_the_other_side + V2 x y
                DUp    -> swapV2 final_position_on_the_other_side + V2 y x
                DDown  -> swapV2 final_position_on_the_other_side + V2 y x
           in EnterLevel (portal^.targetLevel) fixed_position_on_the_other_side
 where
  local_target = direction8ToDelta dir + coords

  findInSet fun set =
    let lst = IS.toList set
     in case find (\portal_id ->
               case level^.portals.at portal_id of
                 Nothing -> False
                 Just ok -> fun ok) lst of
          Nothing -> Nothing
          Just pid -> level^.portals.at pid

  doesItGoThrough portal = couldPotentiallyGoThroughPortal dir (portal^.axis)

data AugmentedCoords = AugmentedCoords !LevelCoordinates !(V2 Int)

levelFieldOfView :: Monad m
                 => LevelCoordinates
                 -> Level
                 -> (LevelID -> Maybe Level)
                 -> (LevelCoordinates -> V2 Int -> Level -> m ())
                 -> m ()
levelFieldOfView coords level get_level i_see =
  void $ flip execStateT level $
    computeFieldOfView
               (\(AugmentedCoords coords offset_coords) -> do
                  lvl <- get
                  lift $ i_see coords offset_coords lvl)
               (\(AugmentedCoords coords _) -> do
                  lvl <- get
                  return $ seeThrough (lvl^.terrainFeature coords))
               ByDirection
               { _leftD      = goThrough D8Left
               , _rightD     = goThrough D8Right
               , _upD        = goThrough D8Up
               , _downD      = goThrough D8Down
               , _leftupD    = goThrough D8UpLeft
               , _leftdownD  = goThrough D8DownLeft
               , _uprightD   = goThrough D8UpRight
               , _downrightD = goThrough D8DownRight }
               (AugmentedCoords coords coords)
               50
               50
 where
  goThrough dir8 (AugmentedCoords coords offset_coords) = do
    result <- goThrough' dir8 coords
    case result of
      Nothing -> return Nothing
      Just ok ->
        let new_offset_coords = offset_coords + direction8ToDelta dir8
         in return $ Just $ AugmentedCoords ok new_offset_coords

  goThrough' dir8 coords = do
    lvl <- get
    case step dir8 coords lvl of
      SameLevel new_coords -> return $ Just new_coords
      EnterLevel new_level_id new_coords ->
        case get_level new_level_id of
          Nothing -> return Nothing
          Just new_level -> do
            put new_level
            return $ Just new_coords
{-# INLINE levelFieldOfView #-}

