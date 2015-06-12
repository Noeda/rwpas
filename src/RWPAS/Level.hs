{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Level
  ( Level()
  , TerrainFeature(..)
  , insertActor
  , emptyLevel
  , roomLevel
  , terrainFeature
  , actors
  , actorById
  , tryMoveActor
  , Size
  , LevelCoordinates
  , levelFieldOfView )
  where

import           Control.Lens hiding ( Level )
import           Data.Data
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
  { _terrain :: Map LevelCoordinates TerrainFeature
  , _actors  :: IntMap Actor }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Coordinates relative to some `Level`.
type LevelCoordinates = V2 Int

-- | Describes the size of something.
type Size = V2 Int

data TerrainFeature
  = Floor
  | Wall
  | Rock   -- ^ Same as `Wall` but completely black.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | If there's no feature at some coordinate, what we should assume it is?
defaultTerrainFeature :: TerrainFeature
defaultTerrainFeature = Rock

-- Derive lenses here
makeLenses ''Level

-- | A completely empty level.
emptyLevel :: Level
emptyLevel = Level { _terrain = mempty
                   , _actors  = mempty }

-- | A level that just has a single rectangular room. The walkable area is
-- sized according to the given coordinates, with (0, 0) being the top-left
-- corner of the room.
roomLevel :: Size -> Level
roomLevel (V2 w h) = Level { _terrain = makeOneRoom w h
                           , _actors  = mempty }
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

impassable :: TerrainFeature -> Bool
impassable Floor = False
impassable _ = True

seeThrough :: TerrainFeature -> Bool
seeThrough Floor = True
seeThrough _ = False

insertActor :: ActorID -> Actor -> Level -> Level
insertActor aid actor = actors.at aid .~ Just actor

tryMoveActor :: ActorID -> Direction -> Level -> Maybe Level
tryMoveActor aid dir level = do
  actor <- IM.lookup aid (level^.actors)
  let actor_pos = actor^.position
      new_actor_pos = directionToDelta dir + actor_pos

  if impassable (level^.terrainFeature new_actor_pos)
    then Nothing
    else Just $ level & actors.at aid .~ Just (actor & position .~ new_actor_pos)

levelFieldOfView :: Monad m
                 => LevelCoordinates
                 -> Level
                 -> (LevelCoordinates -> m ())
                 -> m ()
levelFieldOfView coords level i_see =
  computeFieldOfView i_see
             (\coords -> seeThrough (level^.terrainFeature coords))
             ByDirection
             { _leftD      = \(V2 x y) -> return $ Just $ V2 (x-1) y
             , _rightD     = \(V2 x y) -> return $ Just $ V2 (x+1) y
             , _upD        = \(V2 x y) -> return $ Just $ V2 x (y-1)
             , _downD      = \(V2 x y) -> return $ Just $ V2 x (y+1)
             , _leftupD    = \(V2 x y) -> return $ Just $ V2 (x-1) (y-1)
             , _leftdownD  = \(V2 x y) -> return $ Just $ V2 (x-1) (y+1)
             , _uprightD   = \(V2 x y) -> return $ Just $ V2 (x+1) (y-1)
             , _downrightD = \(V2 x y) -> return $ Just $ V2 (x+1) (y+1) }
             coords
             25
             25
{-# INLINE levelFieldOfView #-}

