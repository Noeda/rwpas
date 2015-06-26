module RWPAS.Control.ListOfAIs
  ( aiList )
  where

import Control.Applicative
import Control.Monad
import Data.Proxy
import Data.SafeCopy
import Data.Serialize.Get
import RWPAS.Control
import RWPAS.Control.BeastFrog

assumeType :: IsAI a => Proxy a -> Get ()
assumeType proxy = do
  len <- getWord8
  bs <- getBytes (fromIntegral len)
  unless (bs == aiName proxy) empty

aiList :: [Get AI]
aiList = [AI <$> (do assumeType (Proxy :: Proxy BeastFrogState)
                     safeGet :: Get BeastFrogState)]

