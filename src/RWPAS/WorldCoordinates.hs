{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.WorldCoordinates
  ( WorldCoordinates(..)
  , levelCoordinates
  , levelID )
  where

import Control.Lens
import Data.Data
import GHC.Generics
import {-# SOURCE #-} RWPAS.Level

data WorldCoordinates = WorldCoordinates
  { _levelCoordinates :: !LevelCoordinates
  , _levelID          :: !LevelID }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''WorldCoordinates

