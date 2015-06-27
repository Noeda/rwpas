{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Actor
  ( sentinelActor
  , Actor()
  , ActorID
  , ActorAppearance(..)
  -- * Appearance, position, AI
  , appearance
  , position
  , actorName
  , ai
  -- * Hit points
  , hurt
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
import Data.SafeCopy
import Data.Text ( Text )
import GHC.Generics
import RWPAS.SafeCopyOrphanInstances()
import {-# SOURCE #-} RWPAS.Control
import {-# SOURCE #-} RWPAS.Control.Types
import Linear.V2

data Actor = Actor
  { _position     :: !ActorPosition
  , _appearance   :: !ActorAppearance
  , _ai           :: !AI
  , _actorName    :: !Text
  , _actorHitPoints :: !(Maybe HitPoints) }
  deriving ( Eq, Ord, Show, Typeable, Generic )

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
deriveSafeCopy 0 'base ''Actor
deriveSafeCopy 0 'base ''ActorAppearance
deriveSafeCopy 0 'base ''HitPoints

class HasActor a where
  actor :: Lens' a Actor

instance HasActor Actor where
  actor = lens id (\_ new -> new)

-- | Sentinel actor. Not meant to be used as-is, take it and modify it to be a
-- proper actor.
sentinelActor :: Text -> Actor
sentinelActor name = Actor
  { _position = V2 0 0
  , _appearance = PlayerCharacter
  , _ai = sentinelAI
  , _actorName = name
  , _actorHitPoints = Nothing }

isDeadByHitPoints :: HasHitPoints a => a -> Bool
isDeadByHitPoints thing =
  thing^.hitPoints.hp <= 0 ||
  thing^.hitPoints.maxHp <= 0

emptyHitPoints :: HitPoints
emptyHitPoints = HitPoints
  { _hp = 0
  , _maxHp = 0 }

hurt :: Int -> Actor -> Actor
hurt points actor = case actor^.actorHitPoints of
  Nothing -> actor
  Just hitp -> actor & actorHitPoints .~ (Just $ hitp & hp -~ points)

hitPointsCritical :: HasHitPoints a => a -> Bool
hitPointsCritical thing =
  thing^.hitPoints.hp <= (thing^.hitPoints.maxHp `div` 3)

hitPointsHealthy :: HasHitPoints a => a -> Bool
hitPointsHealthy thing =
  thing^.hitPoints.hp >= (thing^.hitPoints.maxHp `div` 3 +
                          thing^.hitPoints.maxHp `div` 3)

