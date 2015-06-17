-- | Defines a global `IORef` that contains a list of AIs. This module doesn't
-- list the actual AI implementations, those are files in RWPAS/AIControlledActor/AI/
--

module RWPAS.AIControlledActor.ListOfAIs
  ( aiList )
  where

import Data.SafeCopy
import Data.Serialize.Get
import RWPAS.AIControlledActor.BeastFrog
import RWPAS.AIControlledActor.Types

aiList :: [Get AI]
aiList = [AI <$> (safeGet :: Get BeastFrogState)]

