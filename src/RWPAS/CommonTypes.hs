{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- | This module defines the data types of a RWPAS world.
--
-- They are collected in one module because there are dependencies going back
-- and forth between them. This module exports the data types and automatically
-- generated lenses of them. Other control functions are collected in the
-- respective module, for example, RWPAS.Level for level data.

module RWPAS.CommonTypes
  ( 
  -- * Actors
    Actor(..)
  , ActorID
  , ActorAppearance(..)
  , position
  , appearance
  -- * Portals
  , Portal(..)
  , PortalID
  -- * Levels
  , Level(..)
  , LevelCoordinates
  , LevelID
  , TerrainFeature(..)
  , levelName
  , actors
  , actorKeys
  , actorAIs
  , actorById
  , actorByCoordinates
  , actorMemories
  , portals
  , portalKeys
  , terrain
  -- * AI
  , AI(..)
  , AITransition
  , IsAI(..)
  --  World
  , World(..)
  , HasWorld(..)
  , levelById
  , levels
  , currentFieldOfView
  , currentActor
  , currentLevel )
  where

import           Control.Lens hiding ( Level, levels )
import qualified Data.ByteString as B
import           Data.Data
import           Data.IntMap ( IntMap )
import           Data.IntSet ( IntSet )
import           Data.Map.Strict ( Map )
import           Data.SafeCopy
import           Data.Text ( Text )
import           Data.Word
import           GHC.Generics
import           Linear.V2
import           RWPAS.Direction
import           RWPAS.TwoDimensionalVector
import           System.Random.MWC
import           Unsafe.Coerce

class (SafeCopy a, Eq a, Ord a, Show a, Read a, Typeable a) => IsAI a where
  {-# MINIMAL initialState, transitionFunction, aiName #-}

  initialState :: GenIO -> IO a
  transitionFunction :: AITransition a
  aiName :: Proxy a -> B.ByteString

data Actor = Actor
  { _position     :: !ActorPosition
  , _appearance   :: !ActorAppearance }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data ActorAppearance
  = PlayerCharacter
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

type ActorID = Int

type ActorPosition = V2 Int

data AI = forall a. (IsAI a) => AI a
  deriving ( Typeable )

instance Eq AI where
  AI a1 == AI a2 =
    let r = typeOf a1 == typeOf a2
     in r `seq` (r && unsafeCoerce a1 == a2)

instance Ord AI where
  AI a1 `compare` AI a2 =
    let tc = typeOf a1 `compare` typeOf a2
     in if tc == EQ
          then (unsafeCoerce a1) `compare` a2
          else tc

instance Show AI where
  show (AI a) = "AI<" ++ show a ++ ">"

-- | Function that decides the next action of an AI.
type AITransition a =
     a        -- state of the AI (parametric)
  -> GenIO    -- random number generator
  -> World    -- world state
  -> ActorID  -- actor ID of the actor controlled by this AI
  -> LevelID  -- level ID of the level the actor is in
  -> IO (a, World)

type FieldOfView = Vector2DG (Word8, Word8)

data Level = Level
  { _terrain       :: !Vector2D
  , _portals       :: !(IntMap Portal)
  , _portalKeys    :: !(Map LevelCoordinates IntSet)
  , _actorKeys     :: !(Map LevelCoordinates ActorID)
  , _actorAIs      :: !(IntMap AI)
  , _actors        :: !(IntMap Actor)
  , _actorMemories :: !(Map ActorID (Map LevelCoordinates TerrainFeature))
  , _levelName     :: !Text }
  deriving ( Eq, Ord, Show, Typeable, Generic )

-- | Coordinates relative to some `Level`.
type LevelCoordinates = V2 Int

type LevelID = Int

data TerrainFeature
  = Floor
  | Wall
  | Planks
  | PlanksFloor
  | Tree1
  | Tree2
  | Dirt
  | Grass
  | Rock   -- ^ Same as `Wall` but completely black.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

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

data World = World
  { _levels             :: !(IntMap Level)
  , _currentLevel       :: !LevelID
  , _currentActor       :: !ActorID
  , _currentFieldOfView :: !FieldOfView
  , _runningID          :: !Int }
  deriving ( Eq, Ord, Show, Typeable, Generic )
makeLenses ''Actor
makeLenses ''Level
makeLenses ''World

class HasActor a where
  actor :: Lens' a Actor

class HasWorld a where
  world :: Lens' a World

instance HasWorld World where
  world = lens id (\_ new -> new)

instance HasActor Actor where
  actor = lens id (\_ new -> new)

levelById :: LevelID -> Lens' World (Maybe Level)
levelById lid = levels.at lid

-- | Lens to an actor using some actor ID.
actorById :: ActorID -> Lens' Level (Maybe Actor)
actorById aid = actors.at aid

actorByCoordinates :: LevelCoordinates -> Level -> Maybe ActorID
actorByCoordinates coords level = level^.actorKeys.at coords

