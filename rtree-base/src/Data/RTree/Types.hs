{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.RTree.Types where

import           Control.Lens
import           Data.Data
import           Data.Monoid
import           Data.RTree.Geometry
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Data.Typeable

class (Monad m, HasRectangle t, Ord (RTreePageKey b t) ) =>
      RTreeBackend b (m :: * -> *) t | b -> m, b -> t where
  type RTreePageKey b t :: *
  pageInsert :: b -> RTreePage b t -> m (RTreePageKey b t)
  pageGet :: b -> RTreePageKey b t -> m (RTreePage b t)
  pageGetParentKey :: b -> RTreePageKey b t -> m (RTreePageKey b t)
  pageDelete :: b -> RTreePageKey b t -> m Bool
  pageSetData :: b -> RTreePageKey b t -> Maybe t -> m ()
  pageSetChildren :: b -> RTreePageKey b t -> Set (RTreePageKey b t) -> m ()
  pageSetBoundingBox :: b -> RTreePageKey b t -> Rectangle -> m ()

data RTreePage b t =
  MkRTreePage { _pageData        :: Maybe t
              , _pageChildren    :: Set (RTreePageKey b t)
              , _pageRTreeName   :: Text
              , _pageBoundingBox :: Rectangle
              } deriving (Typeable)
makeClassy ''RTreePage

instance HasRectangle (RTreePage b t) where
  rectangle = pageBoundingBox

data RTree b (m :: * -> *) t =
  MkRTree { _rTreeBackend  :: b
          , _rTreeName     :: Text
          , _rTreeRootNode :: RTreePageKey b t
          } deriving (Typeable)
makeClassy ''RTree

