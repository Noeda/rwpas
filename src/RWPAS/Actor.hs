module RWPAS.Actor
  ( sentinelActor
  , Actor()
  , ActorAppearance(..)
  , position )
  where

import Linear.V2
import RWPAS.CommonTypes

-- | Sentinel actor. Not meant to be used as-is, take it and modify it to be a
-- proper actor.
sentinelActor :: Actor
sentinelActor = Actor
  { _position = V2 0 0
  , _appearance = PlayerCharacter }

