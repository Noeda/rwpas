{-# OPTIONS_GHC -fno-warn-orphans #-}

module RWPAS.AIControlledActor
  ( module RWPAS.AIControlledActor.Types )
  where

import qualified Data.ByteString as B
import Data.Foldable
import Data.Proxy
import Data.SafeCopy
import Data.Serialize.Put
import RWPAS.AIControlledActor.ListOfAIs
import RWPAS.AIControlledActor.Types

proxy :: a -> Proxy a
proxy _ = Proxy

instance SafeCopy AI where
  version = 0
  kind = base

  getCopy = contain $ asum aiList
  putCopy (AI a) = contain $ do
    let n = aiName (proxy $ asProxyTypeOf a Proxy)
    if B.length n > 255
      then error "putCopy (AI): name of AI is too long."
      else do putWord8 (fromIntegral $ B.length n)
              putByteString n
              safePut a

  errorTypeName _ = "AI"
