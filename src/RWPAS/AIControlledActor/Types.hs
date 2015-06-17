{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.AIControlledActor.Types
  ( SentinelAI()
  , AI(..)
  , AITransition
  , IsAI(..)
  -- * Stepping AI
  , stepAI )
  where

import Control.Applicative
import Control.Lens hiding ( Level )
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Data
import Data.SafeCopy
import GHC.Generics
import RWPAS.Actor
import RWPAS.Direction
import RWPAS.CommonTypes
import System.Random.MWC

-- | Sessile AI, used as a placeholder.
data SentinelAI = SentinelAI
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
deriveSafeCopy 0 'base ''SentinelAI

stepAI :: PrimMonad m => AI -> Gen (PrimState m) -> World -> ActorID -> LevelID -> m World
stepAI (AI state) rng world actor_id level_id =
  transitionFunction state rng world actor_id level_id
{-# INLINE stepAI #-}

