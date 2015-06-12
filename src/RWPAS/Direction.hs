{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Direction
  ( Direction(..)
  , directionToDelta )
  where

import Data.Data
import GHC.Generics
import Linear.V2

data Direction
  = DUp
  | DLeft
  | DRight
  | DDown
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

directionToDelta :: Direction -> V2 Int
directionToDelta DUp = V2 0 (-1)
directionToDelta DLeft = V2 (-1) 0
directionToDelta DRight = V2 1 0
directionToDelta DDown = V2 0 1

