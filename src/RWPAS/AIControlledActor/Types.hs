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

import Control.Monad.Primitive
import Data.Data
import Data.SafeCopy
import GHC.Generics
import RWPAS.CommonTypes
import System.Random.MWC

-- | Sessile AI, used as a placeholder.
data SentinelAI = SentinelAI
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
deriveSafeCopy 0 'base ''SentinelAI

stepAI :: PrimMonad m => AI -> Gen (PrimState m) -> World -> ActorID -> LevelID -> m World
stepAI (AI state) = transitionFunction state
{-# INLINE stepAI #-}

