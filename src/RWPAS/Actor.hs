{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Actor
  ( sentinelActor
  , Actor()
  , ActorID
  , ActorAppearance(..)
  , appearance
  , position )
  where

import Control.Lens
import Data.Data
import GHC.Generics
import Linear.V2

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

makeLenses ''Actor

class HasActor a where
  actor :: Lens' a Actor

instance HasActor Actor where
  actor = lens id (\_ new -> new)

-- | Sentinel actor. Not meant to be used as-is, take it and modify it to be a
-- proper actor.
sentinelActor :: Actor
sentinelActor = Actor
  { _position = V2 0 0
  , _appearance = PlayerCharacter }

