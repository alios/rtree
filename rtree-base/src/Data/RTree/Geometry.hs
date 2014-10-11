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


point2Rectangle :: Point -> Rectangle
point2Rectangle p = Rectangle p p

pointZero :: Point
pointZero = Point 0 0

data Rectangle =
  Rectangle { _topLeft     :: Point
            , _bottomRight :: Point
            } deriving (Show, Read, Eq, Ord, Typeable, Data)
makeClassy ''Rectangle


mkRectangle a b =
  let axlt = (a ^. x) <= (b ^. x)
      aylt = (a ^. y) > (b ^. y)
  in case (axlt, aylt) of
       (True, True) -> Rectangle a b
       (False, False) -> Rectangle b a
       (True, False) ->
         let height = abs $ (a ^. y) - (b ^. y)
         in Rectangle { _topLeft = a { _y = (a ^. y) + height }
                      , _bottomRight = b { _y = (b ^. y) - height }}
       (False, True) ->
         let width = abs $ (a ^. x) - (b ^. x)
         in Rectangle { _topLeft = b { _x = (b ^. x) + width }
                      , _bottomRight = a { _x = (a ^. x) - width }}

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
rectWidth r = (r ^. bottomRight . x) - (r ^. topLeft . y)
rectHeight r = (r ^. topLeft . y) - (r ^. bottomRight . y)
rectArea r = (rectWidth r) * (rectHeight r)

rectAreaGrow :: (HasRectangle r1, HasRectangle r2) => r1 -> r2 -> Double
rectAreaGrow a b = (rectArea ((a ^. rectangle) `mappend` (b ^.rectangle))) - rectArea (a ^. rectangle)

rectangleZero :: Rectangle
rectangleZero = point2Rectangle pointZero

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

