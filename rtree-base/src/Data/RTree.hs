
module Data.RTree(
  -- * The RTree frontend
  RTreeFrontend(..),
  RTree,
  -- * The RTree backends
  TabularBackend (..),
  -- * Geometry
  -- ** Rectangle
  Rectangle (..), rectangle, topLeft, bottomRight,
  -- ** Point
  Point (..), point, x, y,
  ) where

import           Data.RTree.Classes
import           Data.RTree.Geometry
import           Data.RTree.Tables
import           Data.RTree.Types


