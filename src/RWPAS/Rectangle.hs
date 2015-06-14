{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Two-dimensional rectangles

module RWPAS.Rectangle
  ( Rectangle()
  , overlaps
  , ltrb
  , enlarge1
  , inside
  , onEdge )
  where

import Data.Data
import GHC.Generics
import Linear.V2

data Rectangle = Rectangle !(V2 Int) !(V2 Int)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Do these two rectangles overlap? Even a little?
overlaps :: Rectangle -> Rectangle -> Bool
overlaps (Rectangle (V2 x1 y1) (V2 x2 y2))
         (Rectangle (V2 x3 y3) (V2 x4 y4)) =
  not $ x2 < x3 || x1 > x4 ||
        y2 < y3 || y1 > y4
{-# INLINE overlaps #-}

-- | Make a rectangle by specifying left-top-right-bottom.
ltrb :: Int -> Int -> Int -> Int -> Rectangle
ltrb x1 y1 x2 y2 = Rectangle (V2 x1 y1) (V2 x2 y2)

-- | Enlarge the rectangle by 1 in every direction.
enlarge1 :: Rectangle -> Rectangle
enlarge1 (Rectangle (V2 x1 y1) (V2 x2 y2)) = Rectangle
  (V2 (x1-1) (y1-1))
  (V2 (x2+1) (y2+1))
{-# INLINE enlarge1 #-}

-- | Is point inside the rectangle.
inside :: V2 Int -> Rectangle -> Bool
inside (V2 x y) (Rectangle (V2 x1 y1) (V2 x2 y2)) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
{-# INLINE inside #-}

-- | Is point at the edge of a rectangle, inside.
onEdge :: V2 Int -> Rectangle -> Bool
onEdge p@(V2 x y) r@(Rectangle (V2 x1 y1) (V2 x2 y2))
  | not (inside p r) = False
  | otherwise = x == x1 || x == x2 || y == y1 || y == y2


