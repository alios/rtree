{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.RTree.Classes where

import           Control.Lens
import           Data.Monoid
import           Data.RTree.Geometry
import           Data.RTree.Types
import           Data.Text           (Text)



class (RTreeBackend b m t) => RTreeFrontend b m t where
  locatePage :: b -> t -> m (RTreePageKey b t)
  splitPage ::  b -> RTreePageKey b t-> m (RTreePage b t, RTreePage b t)
  rTreeCreate :: b -> a -> Text -> m (RTree b m t)
  rTreeInsert :: RTree b m t -> t -> m (RTreePageKey b t)
  rTreeQuery :: (HasRectangle bbox) => RTree b m t -> bbox -> m [t]
  rTreeLoad :: Text -> m (RTree b m t)
  rTreeDestroy :: RTree b m t -> m Bool
  rTreeDelete :: RTree b m t -> RTreePageKey b t -> m Bool

  rTreeQuery tr bbox =
    let b = tr ^. rTreeBackend
        rTreeQuery' p =
          if (not $ p `rectangleIn` bbox)
             then return []
             else case (p ^. pageData) of
                    Just obj -> return [obj]
                    Nothing -> do
                      cps <- sequence . map (pageGet b) $ p ^. pageChildren
                      cps' <- sequence . map rTreeQuery' $ cps
                      return $ concat cps'
    in do
      rp <- pageGet b $ tr ^. rTreeRootNode
      rTreeQuery' rp





{-
class (RTreeBackend b m t, RTreeAlgo a t) => RTreeFrontend b a m t where
  rTreeCreate :: b -> a -> Text -> m (RTree b a m t)
  rTreeCreate b a name =
    let  rootPage :: RTreePage b t
         rootPage =
           MkRTreePage { _pageData = Nothing
                       , _pageChildren = mempty
                       , _pageRTreeName = name
                       , _pageBoundingBox = mempty
                       }
         rtree rootNodeId =
           MkRTree { _rTreeBackend = b
                   , _rTreeAlgo = a
                   , _rTreeName = name
                   , _rTreeRootNode = rootNodeId
                   }
    in pageInsert b rootPage >>= return . rtree
  rTreeInsert :: (HasRectangle t) => RTree b a m t -> t -> m (RTreePageKey b t)
  rTreeInsert tr obj =
    let a = tr ^. rTreeAlgo
        b = tr ^. rTreeBackend
        p = MkRTreePage { _pageData = Just obj
                        , _pageChildren = mempty
                        , _pageRTreeName = tr ^. rTreeName
                        , _pageBoundingBox = obj ^. rectangle
                        }
    in do leafPageId <- locatePage a b obj
          leafPage <- pageGet b leafPageId
          pId <- pageInsert b p
          pageSetBoundingBox b leafPageId $
            (leafPage ^. rectangle) `mappend` (obj ^. rectangle)
          pageSetChildren b leafPageId $ pId : leafPage ^. pageChildren
          return pId
  rTreeQuery :: (HasRectangle bbox) => RTree b a m t -> bbox -> m [t]
  rTreeQuery tr bbox =
    let b = tr ^. rTreeBackend
        rTreeQuery' p =
          if (not $ p `rectangleIn` bbox)
             then return []
             else case (p ^. pageData) of
                    Just obj -> return [obj]
                    Nothing -> do
                      cps <- sequence . map (pageGet b) $ p ^. pageChildren
                      cps' <- sequence . map rTreeQuery' $ cps
                      return $ concat cps'
    in do
      rp <- pageGet b $ tr ^. rTreeRootNode
      rTreeQuery' rp  rTreeLoad :: Text -> m (RTree b a m t)
  rTreeDestroy :: RTree b a m t -> m Bool
  rTreeDelete :: RTree b a m t -> RTreePageKey b t -> m Bool


-}
