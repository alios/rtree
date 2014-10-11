{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.RTree.Geometry where

import           Control.Lens
import           Data.Data     (Data (..))
import           Data.Monoid
import           Data.Typeable (Typeable (..))

data Point =
  Point { _x :: !Double
        , _y :: !Double
        } deriving (Show, Read, Eq, Ord, Typeable, Data)
makeClassy ''Point

data Rectangle =
  Rectangle { _topLeft     :: Point
            , _bottomRight :: Point
            } deriving (Show, Read, Eq, Ord, Typeable, Data)
makeClassy ''Rectangle




mkRectangle' :: Double -> Double -> Double -> Double -> Rectangle
mkRectangle' x1 y1 x2 y2 =
  let top = max y1 y2
      bottom = min y1 y2
      right = max x1 x2
      left = min x1 x2
      tl = Point { _y = top, _x = left}
      br = Point { _y = bottom, _x = right}
  in Rectangle { _topLeft = tl, _bottomRight = br}

mkRectangle :: Point -> Point -> Rectangle
mkRectangle a b = mkRectangle' (a ^. x) (a ^. y) (b ^. x) (b ^. y)


instance Monoid Rectangle where
  mempty = rectangleZero
  mappend r1 r2 =
    let r1left   = r1 ^. topLeft . x
        r1top    = r1 ^. topLeft . y
        r1right  = r1 ^. bottomRight . x
        r1bottom = r1 ^. bottomRight . y
        r2left   = r2 ^. topLeft . x
        r2top    = r2 ^. topLeft . y
        r2right  = r2 ^. bottomRight . x
        r2bottom = r2 ^. bottomRight . y
    in Rectangle { _topLeft     = Point { _x = min r1left r2left, _y = max r1top r2top  }
                 , _bottomRight = Point { _x = max r1right r2right, _y = min r1bottom r2bottom }
                 }


rectWidth, rectHeight, rectArea :: HasRectangle r => r -> Double
rectWidth r  = abs $ (r ^. topLeft . x) - (r ^. bottomRight . x)
rectHeight r = abs $ (r ^. topLeft . y) - (r ^. bottomRight . y)
rectArea r = (rectWidth r) * (rectHeight r)

rectAreaGrow :: (HasRectangle r1, HasRectangle r2) => r1 -> r2 -> Double
rectAreaGrow a b = (rectArea ((a ^. rectangle) `mappend` (b ^.rectangle))) - rectArea (a ^. rectangle)

rectangleIn :: (HasRectangle r1, HasRectangle r2) => r1 -> r2 -> Bool
rectangleIn r1 r2 =
  let r1left   = r1 ^. topLeft . x
      r1top    = r1 ^. topLeft . y
      r1right  = r1 ^. bottomRight . x
      r1bottom = r1 ^. bottomRight . y
      r2left   = r2 ^. topLeft . x
      r2top    = r2 ^. topLeft . y
      r2right  = r2 ^. bottomRight . x
      r2bottom = r2 ^. bottomRight . y
  in not (r2left > r1right || r2right < r1left || r2top <= r1bottom || r2bottom >= r1top)


point2Rectangle :: Point -> Rectangle
point2Rectangle p = mkRectangle p p

pointZero :: Point
pointZero = Point 0 0

rectangleZero :: Rectangle
rectangleZero = point2Rectangle pointZero
