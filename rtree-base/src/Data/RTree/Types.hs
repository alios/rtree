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
{-# LANGUAGE UndecidableInstances   #-}

module Data.RTree.Types where

import           Control.Lens
import           Data.Data
import           Data.RTree.Geometry
import           Data.Set            (Set)
import           Data.Text           (Text)

-- | a backend stores pages.
--   * a root page.
--   * a page might be da data page where it stores objects of type 't'
--   * a page that stores child pages
--   every bage has a bounding box which covers all other objects its subpages
class (Monad m, HasRectangle t, Ord (RTreePageKey b t) ) =>
      RTreeBackend b (m :: * -> *) t | b -> m, b -> t where
  type RTreePageKey b t :: *
  -- | create a new empty 'RTreeBackend' instance
  backendCreate :: m b
  -- | insert a 'RTreePage' into the 'RTreeBackend'.
  pageInsert :: b -> RTreePage b t -> m (RTreePageKey b t)
  -- | get a 'RTreePage' from 'RTReeBackend' indexed by its 'RTreeKey'.
  pageGet :: b -> RTreePageKey b t -> m (RTreePage b t)
  -- | get the 'RTreePageKay' of a pages parent or 'Nothing'
  --   if the page does not exist or is the root page.
  pageGetParentKey :: b -> RTreePageKey b t -> m (Maybe (RTreePageKey b t))
  -- | delete a page
  pageDelete :: b -> RTreePageKey b t -> m Bool
  -- | set the pages data
  pageSetData :: b -> RTreePageKey b t -> Maybe t -> m ()
  -- | set the pages children
  pageSetChildren :: b -> RTreePageKey b t -> Set (RTreePageKey b t) -> m ()
  -- | set the pages bounding box
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

