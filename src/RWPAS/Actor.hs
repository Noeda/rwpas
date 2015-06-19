{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Actor
  ( sentinelActor
  , Actor()
  , ActorID
  , ActorAppearance(..)
  -- * Appearance and position
  , appearance
  , position
  -- * Hit points
  , actorHitPoints
  , emptyHitPoints
  , hitPointsCritical
  , hitPointsHealthy
  , HasHitPoints(..)
  , HitPoints()
  , isDeadByHitPoints )
  where

import Control.Lens
import Data.Data
import GHC.Generics
import Linear.V2

data Actor = Actor
  { _position     :: !ActorPosition
  , _appearance   :: !ActorAppearance

  , _actorHitPoints :: !(Maybe HitPoints) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data ActorAppearance
  = PlayerCharacter
  | BeastFrog
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

type ActorID = Int

type ActorPosition = V2 Int

data HitPoints = HitPoints
  { _hp :: !Int
  , _maxHp :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

makeLenses ''Actor
makeClassy ''HitPoints

class HasActor a where
  actor :: Lens' a Actor

instance HasActor Actor where
  actor = lens id (\_ new -> new)

-- | Sentinel actor. Not meant to be used as-is, take it and modify it to be a
-- proper actor.
sentinelActor :: Actor
sentinelActor = Actor
  { _position = V2 0 0
  , _appearance = PlayerCharacter
  , _actorHitPoints = Nothing }

isDeadByHitPoints :: HasHitPoints a => a -> Bool
isDeadByHitPoints thing =
  thing^.hitPoints.hp <= 0 ||
  thing^.hitPoints.maxHp <= 0

emptyHitPoints :: HitPoints
emptyHitPoints = HitPoints
  { _hp = 0
  , _maxHp = 0 }

hitPointsCritical :: HasHitPoints a => a -> Bool
hitPointsCritical thing =
  thing^.hitPoints.hp <= (thing^.hitPoints.maxHp `div` 3)

hitPointsHealthy :: HasHitPoints a => a -> Bool
hitPointsHealthy thing =
  thing^.hitPoints.hp >= (thing^.hitPoints.maxHp `div` 3 +
                          thing^.hitPoints.maxHp `div` 3)

