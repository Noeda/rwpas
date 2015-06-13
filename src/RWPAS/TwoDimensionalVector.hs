{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.TwoDimensionalVector
  ( Vector2D()
  , getAt
  , generate )
  where

import           Control.Lens
import           Data.Data
import           Data.Vector.Unboxed ( Vector )
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           GHC.Generics
import           Linear.V2

data Vector2D = Vector2D
  { _vec     :: {-# UNPACK #-} !(Vector Word8)
  , _width :: !Int
  , _height :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''Vector2D

getAt :: V2 Int -> Vector2D -> Word8 -> Word8
getAt (V2 x y) vector default_answer =
  if x < 0 || y < 0 || x >= (vector^.width) || y >= (vector^.height)
    then default_answer
    else V.unsafeIndex (vector^.vec) (x + y*(vector^.width))
{-# INLINE getAt #-}

generate :: Int -> Int -> (Int -> Int -> Word8) -> Vector2D
generate w h generator = Vector2D
  { _vec = generatedVector
  , _width = w
  , _height = h }
 where
  generatedVector = V.generate (w*h) $ \o ->
    let x = o `mod` w
        y = o `div` w
     in generator x y

