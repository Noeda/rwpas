{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Direction
  ( Direction4(..)
  , Direction8(..)
  , direction4ToDelta
  , direction8ToDelta
  , direction4To8 )
  where

import Data.Data
import GHC.Generics
import Linear.V2

-- | The four directions.
data Direction4
  = DUp
  | DLeft
  | DRight
  | DDown
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | The eight directions.
data Direction8
  = D8Up
  | D8Left
  | D8Right
  | D8Down
  | D8UpLeft
  | D8UpRight
  | D8DownLeft
  | D8DownRight
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

direction4ToDelta :: Direction4 -> V2 Int
direction4ToDelta DUp = V2 0 (-1)
direction4ToDelta DLeft = V2 (-1) 0
direction4ToDelta DRight = V2 1 0
direction4ToDelta DDown = V2 0 1

direction8ToDelta :: Direction8 -> V2 Int
direction8ToDelta D8Up = V2 0 (-1)
direction8ToDelta D8Left = V2 (-1) 0
direction8ToDelta D8Right = V2 1 0
direction8ToDelta D8Down = V2 0 1
direction8ToDelta D8UpLeft = V2 (-1) (-1)
direction8ToDelta D8UpRight = V2 1 (-1)
direction8ToDelta D8DownLeft = V2 (-1) 1
direction8ToDelta D8DownRight = V2 1 1

direction4To8 :: Direction4 -> Direction8
direction4To8 DUp = D8Up
direction4To8 DDown = D8Down
direction4To8 DLeft = D8Left
direction4To8 DRight = D8Right

