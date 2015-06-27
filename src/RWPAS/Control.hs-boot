{-# OPTIONS_GHC -fno-warn-orphans #-}

module RWPAS.Control
  ( AI() ) where
  import Data.SafeCopy
  import {-# SOURCE #-} RWPAS.Control.Types

  instance SafeCopy AI

