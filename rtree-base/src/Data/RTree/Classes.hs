{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.RTree.Classes where

import           Control.Lens
import           Data.Monoid
import           Data.RTree.Geometry
import           Data.RTree.Types
import           Data.Text           (Text)

class (RTreeBackend b m, RTreeAlgo a) => RTreeFrontend b a m | b -> m where
  rTreeLoad :: Text -> m (RTree b a m t)
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
  rTreeDestroy :: RTree b a m t -> m Bool
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
          pageSetChildren b leafPageId $ pId : leafPage ^. pageChildren
          pageSetBoundingBox b leafPageId $
            (leafPage ^. rectangle) `mappend` (obj ^. rectangle)
          return pId
  rTreeDelete :: RTree b a m t -> RTreePageKey b t -> m Bool
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
      rTreeQuery' rp



data RTreeStar = RTreeStar
instance RTreeAlgo RTreeStar where



