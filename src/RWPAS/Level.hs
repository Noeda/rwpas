{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module RWPAS.Level
  ( Level()
  , TerrainFeature(..)
  , emptyLevel
  , roomLevel
  , terrainFeature
  , Size
  , LevelCoordinates )
  where

import           Control.Lens hiding ( Level )
import           Data.Data
import qualified Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import           Data.Maybe
import           GHC.Generics
import           Linear.V2

data Level = Level
  { _terrain :: M.Map LevelCoordinates TerrainFeature }
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
emptyLevel = Level { _terrain = mempty }

-- | A level that just has a single rectangular room. The walkable area is
-- sized according to the given coordinates, with (0, 0) being the top-left
-- corner of the room.
roomLevel :: Size -> Level
roomLevel (V2 w h) = Level { _terrain = makeOneRoom w h }
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

