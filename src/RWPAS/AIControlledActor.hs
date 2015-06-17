{-# OPTIONS_GHC -fno-warn-orphans #-}

module RWPAS.AIControlledActor
  ( module RWPAS.AIControlledActor.Types )
  where

import Data.Foldable
import Data.SafeCopy
import RWPAS.AIControlledActor.ListOfAIs
import RWPAS.AIControlledActor.Types

instance SafeCopy AI where
  version = 0
  kind = base

  getCopy = contain $ asum aiList
  putCopy (AI a) = contain $ safePut a

  errorTypeName _ = "AI"

