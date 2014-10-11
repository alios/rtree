{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.RTree.Types where

import           Control.Lens
import           Data.RTree.Geometry
import           Data.Text           (Text)


class RTreeAlgo a where
  locatePage :: (RTreeBackend b m, HasRectangle bbox) => a ->  b -> bbox -> m (RTreePageKey b t)
  splitPage ::  (RTreeBackend b m) => a -> b -> RTreePageKey b t-> m (RTreePage b t, RTreePage b t)

class (Monad m) => RTreeBackend b (m :: * -> *) | b -> m where
  type RTreePageKey b :: * -> *
  pageInsert :: b -> RTreePage b t -> m (RTreePageKey b t)
  pageGet :: b -> RTreePageKey b t -> m (RTreePage b t)
  pageDelete :: b -> RTreePageKey b t -> m Bool
  pageSetData :: b -> RTreePageKey b t -> Maybe t -> m ()
  pageSetChildren :: b -> RTreePageKey b t -> [RTreePageKey b t]  -> m ()
  pageSetBoundingBox :: (HasRectangle bbox) => b -> RTreePageKey b t -> bbox -> m ()


data RTreePage b t =
  MkRTreePage { _pageData        :: Maybe t
              , _pageChildren    :: [RTreePageKey b t]
              , _pageRTreeName   :: Text
              , _pageBoundingBox :: Rectangle
              }
makeClassy ''RTreePage

instance HasRectangle (RTreePage b t) where
  rectangle = pageBoundingBox

data RTree b a (m :: * -> *) t =
  MkRTree { _rTreeBackend  :: b
          , _rTreeAlgo     :: a
          , _rTreeName     :: Text
          , _rTreeRootNode :: RTreePageKey b t
          }
makeClassy ''RTree

