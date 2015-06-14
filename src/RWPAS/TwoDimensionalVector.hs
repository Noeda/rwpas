{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.TwoDimensionalVector
  ( Vector2D()
  , Vector2DG()
  , Vector2DGMut()
  , getAt
  , generate
  , generateM
  , viewWidth
  , viewHeight
  , newMutable
  , writeToMutable
  , unsafeFreezeVector2D )
  where

import           Control.Lens
import           Control.Monad.ST
import           Data.Data
import           Data.Vector.Unboxed ( Vector )
import           Data.Vector.Unboxed.Mutable
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           GHC.Generics
import           Linear.V2

data Vector2DG a = Vector2DG
  { _vec    :: !(Vector a)
  , _width  :: !Int
  , _height :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Vector2DGMut s a = Vector2DGMut
  { _mvec    :: !(MVector s a)
  , _mwidth  :: !Int
  , _mheight :: !Int }
makeLenses ''Vector2DG
makeLenses ''Vector2DGMut

type Vector2D = Vector2DG Word8

viewWidth :: Vector2DG a -> Int
viewWidth vec = vec^.width

viewHeight :: Vector2DG a -> Int
viewHeight vec = vec^.height

getAt :: Unbox a => V2 Int -> Vector2DG a -> a -> a
getAt (V2 x y) vector default_answer =
  if x < 0 || y < 0 || x >= (vector^.width) || y >= (vector^.height)
    then default_answer
    else V.unsafeIndex (vector^.vec) (x + y*(vector^.width))
{-# INLINE getAt #-}

unsafeFreezeVector2D :: Unbox a => Vector2DGMut s a -> ST s (Vector2DG a)
unsafeFreezeVector2D vec = do
  arr <- V.unsafeFreeze (vec^.mvec)
  return Vector2DG
    { _vec = arr
    , _width = vec^.mwidth
    , _height = vec^.mheight }

newMutable :: Unbox a => Int -> Int -> a -> ST s (Vector2DGMut s a)
newMutable w h initial = do
  arr <- Data.Vector.Unboxed.Mutable.replicate (w*h) initial
  return Vector2DGMut
    { _mvec = arr
    , _mwidth = w
    , _mheight = h }

writeToMutable :: Unbox a => Vector2DGMut s a -> Int -> Int -> a -> ST s ()
writeToMutable vector x y item =
  if x >= 0 && y >= 0 && x < vector^.mwidth && y < vector^.mheight
    then unsafeWrite (vector^.mvec) (x + y*vector^.mwidth) item
    else error $ "writeToMutable: out of range (size is (" ++ show (vector^.mwidth) ++ ", " ++ show (vector^.mheight) ++ "), tried to write to (" ++ show x ++ ", " ++ show y ++ ")"
{-# INLINE writeToMutable #-}

generate :: Unbox a => Int -> Int -> (Int -> Int -> a) -> Vector2DG a
generate w h generator = Vector2DG
  { _vec = generatedVector
  , _width = w
  , _height = h }
 where
  generatedVector = V.generate (w*h) $ \o ->
    let x = o `mod` w
        y = o `div` w
     in generator x y

generateM :: (Monad m, Unbox a) => Int -> Int -> (Int -> Int -> m a) -> m (Vector2DG a)
generateM w h generator = do
  generated_vec <- V.generateM (w*h) $ \o ->
    let x = o `mod` w
        y = o `div` w
     in generator x y

  return Vector2DG
    { _vec = generated_vec
    , _width = w
    , _height = h }

