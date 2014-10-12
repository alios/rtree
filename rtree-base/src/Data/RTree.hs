
module Data.RTree
       ( RTreeFrontend(..)
       , RTreeBackend, RTreeAlgo, RTree, RTreePage, RTreePageKey
       , Point (..), point, x, y, point2Rectangle
       , Rectangle (..), mkRectangle, rectangle, topLeft, bottomRight
       , rectangleIn, rectWidth, rectHeight, rectArea, rectAreaGrow
       ) where

import           Data.RTree.Classes
import           Data.RTree.Geometry
import           Data.RTree.Tables
import           Data.RTree.Types


