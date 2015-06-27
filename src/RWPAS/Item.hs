{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Item
  ( ItemAppearance(..)
  , Item()
  , stackSize
  , itemAppearance
  , itemName
  , sentinelItem
  -- * Stacking
  , tryStack )
  where

import Control.Lens
import Data.Data
import Data.SafeCopy
import Data.Text
import GHC.Generics

data ItemAppearance
  = BeastFrogCorpse
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Item = Item
  { _stackSize :: !Int
  , _itemAppearance :: !ItemAppearance
  , _itemName :: !Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''Item
deriveSafeCopy 0 'base ''Item
deriveSafeCopy 0 'base ''ItemAppearance

sentinelItem :: Text -> Item
sentinelItem name = Item
  { _stackSize = 1
  , _itemAppearance = BeastFrogCorpse
  , _itemName = name }

tryStack :: Item -> Item -> Maybe Item
tryStack item1 item2
  | item1^.itemAppearance == item2^.itemAppearance &&
    item1^.itemName == item2^.itemName = Just $
      item1 & stackSize +~ (item2^.stackSize)
  | otherwise = Nothing

