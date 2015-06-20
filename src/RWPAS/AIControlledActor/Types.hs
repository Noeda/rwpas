{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.AIControlledActor.Types
  ( SentinelAI()
  , sentinelAI
  , AI(..)
  , AITransition
  , IsAI(..)
  -- * Stepping AI
  , stepAI )
  where

import Control.Monad.Primitive
import qualified Data.ByteString as B
import Data.Data
import Data.SafeCopy
import GHC.Generics
import {-# SOURCE #-} RWPAS.Control
import RWPAS.Actor
import {-# SOURCE #-} RWPAS.Level
import System.Random.MWC
import Unsafe.Coerce

class (SafeCopy a, Eq a, Ord a, Show a, Read a, Typeable a) => IsAI a where
  {-# MINIMAL initialState, transitionFunction, aiName #-}

  initialState :: PrimMonad m => Gen (PrimState m) -> m a
  transitionFunction :: PrimMonad m => AITransition m a
  aiName :: Proxy a -> B.ByteString

-- | Function that decides the next action of an AI.
type AITransition m a =
     a        -- state of the AI (parametric)
  -> Gen (PrimState m)    -- random number generator
  -> World    -- world state
  -> ActorID  -- actor ID of the actor controlled by this AI
  -> LevelID  -- level ID of the level the actor is in
  -> m World

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
          then unsafeCoerce a1 `compare` a2
          else tc

instance Show AI where
  show (AI a) = "AI<" ++ show a ++ ">"

-- | Sessile AI, used as a placeholder.
data SentinelAI = SentinelAI
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
deriveSafeCopy 0 'base ''SentinelAI

instance IsAI SentinelAI where
  initialState _ = return SentinelAI
  transitionFunction _ _ w _ _ = return w
  aiName _ = "sessile AI"

stepAI :: PrimMonad m => AI -> Gen (PrimState m) -> World -> ActorID -> LevelID -> m World
stepAI (AI state) = transitionFunction state
{-# INLINE stepAI #-}

sentinelAI :: AI
sentinelAI = AI SentinelAI

