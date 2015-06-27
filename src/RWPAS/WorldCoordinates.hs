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
import Linear.V2

data WorldCoordinates = WorldCoordinates
  { _levelCoordinates :: !(V2 Int) -- LevelCoordinates
  , _levelID          :: !Int }    -- LevelID
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''WorldCoordinates

