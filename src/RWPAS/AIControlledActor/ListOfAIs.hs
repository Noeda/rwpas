-- | Defines a global `IORef` that contains a list of AIs. This module doesn't
-- list the actual AI implementations, those are files in RWPAS/AIControlledActor/AI/
--

module RWPAS.AIControlledActor.ListOfAIs
  ( aiList )
  where

import Control.Applicative
import Control.Monad
import Data.Proxy
import Data.SafeCopy
import Data.Serialize.Get
import RWPAS.AIControlledActor.BeastFrog
import RWPAS.AIControlledActor.Types

assumeType :: IsAI a => Proxy a -> Get ()
assumeType proxy = do
  len <- getWord8
  bs <- getBytes (fromIntegral len)
  unless (bs == aiName proxy) empty

aiList :: [Get AI]
aiList = [AI <$> (do assumeType (Proxy :: Proxy BeastFrogState)
                     safeGet :: Get BeastFrogState)]

