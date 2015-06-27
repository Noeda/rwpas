-- | Defines SafeCopy instances for types from other packages that don't
-- implement it themselves.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RWPAS.SafeCopyOrphanInstances
  ( )
  where

import Linear.V2
import Data.SafeCopy

deriveSafeCopy 0 'base ''V2

