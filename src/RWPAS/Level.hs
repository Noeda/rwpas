{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module RWPAS.Level
  ( Level()
  -- * Level construction
  , generateLevel
  , generateLevelM
  , emptyLevel
  , roomLevel
  , portalOnRightSideLevel
  , addPortal
  , terrainFeature
  , TerrainFeature(..)
  , levelName
  , levelSize
  , impassable
  -- * Simulation
  , cycleLevel
  -- * Decorations
  , Decoration(..)
  , decorationByCoordinate
  -- * Actor handling
  --
  -- Some of these functions are in RWPAS.Control instead that's a bit higher
  -- level than these.
  , getMemoryAt
  , insertActor
  , tryMoveActor
  , removeActor
  , actors
  , actorById
  , actorByCoordinates
  , updateActorMemories
  , bestowAI
  -- * Types, coordinates, sizes
  , LevelCoordinates
  , Size
  , LevelID
  -- * Computing field of view
  , levelFieldOfView
  -- * Stepping
  , step
  , StepResult(..) )
  where

import           Control.Lens hiding ( Level )
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Data.Data
import           Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text ( Text )
import           GHC.Generics
import           Linear.V2
import           RWPAS.Actor
import           RWPAS.AIControlledActor
import           RWPAS.CommonTypes
import           RWPAS.Direction
import           RWPAS.FieldOfView
import           RWPAS.TwoDimensionalVector
import           System.Random.MWC

-- | Describes the size of something.
type Size = V2 Int

-- Derive lenses here
makeLenses ''Portal

-- | If there's no feature at some coordinate, what we should assume it is?
defaultTerrainFeature :: TerrainFeature
defaultTerrainFeature = Rock

getMemoryAt :: ActorID -> Level -> LevelCoordinates -> Maybe TerrainFeature
getMemoryAt aid level coords = do
  memory <- level^.actorMemories.at aid
  memory^.at coords

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

decorationByCoordinate :: LevelCoordinates -> Lens' Level Decoration
decorationByCoordinate coords = lens get_it set_it
 where
  get_it lvl = fromMaybe NotDecorated (lvl^.decorations.at coords)
  set_it lvl NotDecorated = lvl & decorations.at coords .~ Nothing
  set_it lvl x = lvl & decorations.at coords .~ Just x
{-# INLINE decorationByCoordinate #-}

-- | Generate a level with a generator function.
generateLevel :: Text -> Int -> Int -> (Int -> Int -> TerrainFeature) -> Level
generateLevel name w h generator = (emptyLevel name)
  { _terrain = generate w h $ \x y -> fromIntegral $ fromEnum $ generator x y }

generateLevelM :: Monad m
               => Text
               -> Int
               -> Int
               -> (Int -> Int -> m TerrainFeature)
               -> m Level
generateLevelM name w h generator = do
  generated <- generateM w h $ \x y -> fromIntegral . fromEnum <$> generator x y
  return (emptyLevel name) { _terrain = generated }

-- | A completely empty level.
emptyLevel :: Text -> Level
emptyLevel name = Level { _terrain    = generate 1 1 $ \_ _ -> fromIntegral $ fromEnum defaultTerrainFeature
                   , _actors        = mempty
                   , _actorMemories = mempty
                   , _actorKeys     = mempty
                   , _actorAIs      = mempty
                   , _portals       = mempty
                   , _portalKeys    = mempty
                   , _decorations   = mempty
                   , _levelName     = name }

updateActorMemories :: ActorID -> M.Map LevelCoordinates TerrainFeature -> Level -> Level
updateActorMemories aid memories levels =
  case levels^.actorMemories.at aid of
    Nothing -> levels & actorMemories.at aid .~ Just memories
    Just m  -> levels & actorMemories.at aid .~ Just (M.union memories m)

-- | Same as `roomLevel` but adds a portal to the right side of the room.
--
-- The portal leads to the left side of the room. Pass the same `LevelID` as
-- the level itself.
portalOnRightSideLevel :: Size -> PortalID -> PortalID -> LevelID -> Level
portalOnRightSideLevel sz@(V2 w h) pid pid2 lid =
  let initial_level = roomLevel sz
   in addPortal Portal { _axis = DLeft
                       , _targetLevel = lid
                       , _targetLevelAxisTopPosition = 1
                       , _targetLevelAxisPosition    = w-1
                       , _portalLength = h-1
                       , _axisTopPosition = 1
                       , _axisPosition = 1 }
                pid2 $
      addPortal Portal { _axis = DRight
                       , _targetLevel = lid
                       , _targetLevelAxisTopPosition = 1
                       , _targetLevelAxisPosition = 1
                       , _portalLength = h-1
                       , _axisTopPosition = 1
                       , _axisPosition = w-1 }
                pid
                initial_level

-- | A level that just has a single rectangular room. The walkable area is
-- sized according to the given coordinates, with (1, 1) being the top-left
-- corner of the room and (0, 0) is top-left wall.
roomLevel :: Size -> Level
roomLevel (V2 w h) = Level { _terrain       = makeOneRoom w h
                           , _actors        = mempty
                           , _actorKeys     = mempty
                           , _actorAIs      = mempty
                           , _actorMemories = mempty
                           , _portals       = mempty
                           , _portalKeys    = mempty
                           , _decorations   = mempty
                           , _levelName     = "Rectangular Room" }
 where
  makeOneRoom w h = generate (w+1) (h+1) $ \x y ->
    if | x == 0 || y == 0 || x == w || y == h
         -> fromIntegral $ fromEnum Wall
       | x == w `div` 2 && y > 5 && y < h-6
         -> fromIntegral $ fromEnum Wall
       | otherwise -> fromIntegral $ fromEnum Floor

-- | Lens to a terrain feature at some location.
terrainFeature :: LevelCoordinates -> Level -> TerrainFeature
terrainFeature coords level =
  toEnum $ fromIntegral $ getAt coords (level^.terrain) (fromIntegral $ fromEnum Rock)
{-# INLINE terrainFeature #-}

impassable :: TerrainFeature -> Bool
impassable Floor  = False
impassable Dirt   = False
impassable Grass = False
impassable PlanksFloor = False
impassable _ = True

seeThrough :: TerrainFeature -> Int
seeThrough Floor = 0
seeThrough Dirt  = 0
seeThrough Grass = 0
seeThrough PlanksFloor = 0
seeThrough Tree1 = 1
seeThrough Tree2 = 1
seeThrough _ = 10000

tryMoveActor :: ActorID -> Direction8 -> LevelID -> Level -> (LevelID -> Maybe Level) -> Maybe (Level, Maybe (LevelID, Level))
tryMoveActor aid dir source_level_id level get_level = do
  actor <- IM.lookup aid (level^.actors)
  let actor_pos = actor^.position

  case step dir actor_pos level of
    SameLevel new_actor_pos ->
      if impassable (terrainFeature new_actor_pos level) ||
         isJust (actorByCoordinates new_actor_pos level)
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
        Just new_level -> if impassable (terrainFeature new_actor_pos new_level) ||
                             isJust (actorByCoordinates new_actor_pos new_level)
            then Nothing
            else Just (level & (actors.at aid .~ Nothing) .
                               (actorKeys.at actor_pos .~ Nothing) .
                               (actorAIs.at aid .~ Nothing)
                      ,Just (new_level_id
                            ,new_level &
                             (actorAIs.at aid .~ (level^.actorAIs.at aid)) .
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

-- | Steps one unit to some direction in a level. Returns a `StepResult` that
-- tells if the step moved through a portal or stayed on the same level.
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
                 => Int
                 -> Int
                 -> LevelCoordinates
                 -> Level
                 -> LevelID
                 -> (LevelID -> Maybe Level)
                 -> (LevelCoordinates -> V2 Int -> Level -> LevelID -> m ())
                 -> m ()
levelFieldOfView x_extent y_extent coords level level_id get_level i_see =
  void $ flip execStateT (level, level_id) $
    computeFieldOfView
               (\(AugmentedCoords coords offset_coords) -> do
                  (lvl, lvl_id) <- get
                  lift $ i_see coords offset_coords lvl lvl_id)
               (\(AugmentedCoords coords _) -> do
                  (lvl, _) <- get
                  return $ seeThrough (terrainFeature coords lvl))
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
               2
               x_extent
               y_extent
 where
  goThrough dir8 (AugmentedCoords coords offset_coords) = do
    result <- goThrough' dir8 coords
    case result of
      Nothing -> return Nothing
      Just ok ->
        let new_offset_coords = offset_coords + direction8ToDelta dir8
         in return $ Just $ AugmentedCoords ok new_offset_coords

  goThrough' dir8 coords = do
    (lvl, _) <- get
    case step dir8 coords lvl of
      SameLevel new_coords -> return $ Just new_coords
      EnterLevel new_level_id new_coords ->
        case get_level new_level_id of
          Nothing -> return Nothing
          Just new_level -> do
            put (new_level, new_level_id)
            return $ Just new_coords
{-# INLINE levelFieldOfView #-}

-- | Removes all decorations from a level.
removeDecorations :: Level -> Level
removeDecorations lvl = lvl & decorations .~ mempty

-- | Simulates all actors on the level for one cycle.
cycleLevel :: PrimMonad m => LevelID -> World -> Gen (PrimState m) -> m World
cycleLevel level_id world rng =
  case world^.levelById level_id of
    Nothing -> return world
    Just (removeDecorations -> lvl) -> flip execStateT world $ do
      ifor_ (lvl^.actorAIs) $ \aid ai -> do
        w <- get
        case w^.levelById level_id of
          Nothing -> return ()
          Just lvl -> case lvl^.actorById aid of
            Nothing -> return ()
            Just _ -> do
              new_world <- stepAI ai rng w aid level_id
              put new_world

