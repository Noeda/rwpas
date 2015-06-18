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
  , levelSize
  , actors
  , actorKeys
  , actorAIs
  , actorById
  , actorByCoordinates
  , actorMemories
  , decorations
  , portals
  , portalKeys
  , terrain
  , insertActor
  , removeActor
  , bestowAI
  -- * Decoration
  , Decoration(..)
  -- * AI
  , AI(..)
  , AITransition
  , IsAI(..)
  --  World
  , World(..)
  , HasWorld(..)
  , RunningID
  , runningID
  , levelById
  , levels
  , currentFieldOfView
  , currentActor
  , currentLevel
  , diagonalDistance )
  where

import           Control.Monad.Primitive
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

  initialState :: PrimMonad m => Gen (PrimState m) -> m a
  transitionFunction :: PrimMonad m => AITransition m a
  aiName :: Proxy a -> B.ByteString

data Actor = Actor
  { _position     :: !ActorPosition
  , _appearance   :: !ActorAppearance }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data ActorAppearance
  = PlayerCharacter
  | BeastFrog
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
type AITransition m a =
     a        -- state of the AI (parametric)
  -> Gen (PrimState m)    -- random number generator
  -> World    -- world state
  -> ActorID  -- actor ID of the actor controlled by this AI
  -> LevelID  -- level ID of the level the actor is in
  -> m World

type FieldOfView = Vector2DG (Word8, Word8, Word8)

data Decoration
  = Spikes !Direction8
  | NotDecorated
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance Enum Decoration where
  toEnum x | x > 1 && x <= 8 =
    let dir = toEnum (x-1) :: Direction8
     in Spikes dir
  toEnum 0 = NotDecorated
  toEnum _ = error "toEnum (Decoration): invalid value"
  {-# INLINE toEnum #-}

  fromEnum (Spikes dir) = fromEnum dir + 1
  fromEnum NotDecorated = 0
  {-# INLINE fromEnum #-}

data Level = Level
  { _terrain       :: !Vector2D
  , _decorations   :: !(Map LevelCoordinates Decoration)
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

type RunningID = Int

data World = World
  { _levels             :: !(IntMap Level)
  , _currentLevel       :: !LevelID
  , _currentActor       :: !ActorID
  , _currentFieldOfView :: !FieldOfView
  , _runningID          :: !RunningID }
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

levelSize :: Level -> V2 Int
levelSize lvl = V2 (viewWidth (lvl^.terrain)) (viewHeight (lvl^.terrain))
{-# INLINE levelSize #-}

-- | Returns the diagonal distance between two coordinates.
diagonalDistance :: V2 Int -> V2 Int -> Int
diagonalDistance (V2 x1 y1) (V2 x2 y2) =
  max (abs $ x1-x2) (abs $ y1-y2)
{-# INLINE diagonalDistance #-}

-- | Inserts an actor somewhere on the level.
--
-- Actor already at the target position is overwritten, if there was anything
-- there.
insertActor :: ActorID -> Actor -> Level -> Level
insertActor aid actor =
  (actors.at aid .~ Just actor) .
  (actorKeys.at (actor^.position) .~ Just aid)

-- | Gives an artificial intelligence to an actor.
--
-- Does nothing if the given actor is not on the level.
bestowAI :: ActorID -> AI -> Level -> Level
bestowAI aid ai level =
  case level^.actorById aid of
    Nothing -> level
    Just _ -> level & actorAIs.at aid .~ Just ai

-- | Removes an actor from the level.
--
-- Does nothing if the actor is not in the level.
removeActor :: ActorID -> Level -> Level
removeActor aid level =
  case level^.actors.at aid of
    Nothing -> level
    Just actor ->
      level & (actors.at aid .~ Nothing) .
              (actorKeys.at (actor^.position) .~ Nothing) .
              (actorAIs.at aid .~ Nothing)

