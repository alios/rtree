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



class (RTreeBackend b m t) => RTreeFrontend a b m t where
  locatePage :: a -> b -> t -> RTreePageKey b t -> m (RTreePageKey b t)
  splitPage ::  a -> b -> t -> RTreePageKey b t-> m (RTreePageKey b t)
  maxChildren :: a -> b -> Int
  rTreeLoad :: Text -> m (RTree b m t)
  rTreeDestroy :: RTree b m t -> m Bool
  rTreeDelete :: RTree b m t -> RTreePageKey b t -> m Bool

  rTreeQuery :: (HasRectangle bbox) => RTree b m t -> bbox -> m [t]
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

  rTreeCreate :: b -> Text -> m (RTree b m t)
  rTreeCreate b name =
    let  rootPage =
           MkRTreePage { _pageData = Nothing
                       , _pageChildren = mempty
                       , _pageRTreeName = name
                       , _pageBoundingBox = mempty
                       }
         rtree rootNodeId =
               MkRTree { _rTreeBackend = b
                       , _rTreeName = name
                       , _rTreeRootNode = rootNodeId
                       }
    in do pageInsert b rootPage >>= return . rtree

  rTreeInsert :: a -> RTree b m t -> t -> m (RTreePageKey b t)
  rTreeInsert a tr obj =
    let b = tr ^. rTreeBackend
        p = MkRTreePage { _pageData = Just obj
                        , _pageChildren = mempty
                        , _pageRTreeName = tr ^. rTreeName
                        , _pageBoundingBox = obj ^. rectangle
                        }
        doInsert leafPageId leafPage = do
          pId <- pageInsert b p
          pageSetBoundingBox b leafPageId $
            (leafPage ^. rectangle) `mappend` (obj ^. rectangle)
          pageSetChildren b leafPageId $ pId : leafPage ^. pageChildren
          return pId
    in do leafPageId <- locatePage a b obj $ tr ^. rTreeRootNode
          leafPage <- pageGet b leafPageId
          if ((leafPage ^. pageChildren . to length) >= maxChildren a b)
             then do leafPageId' <- splitPage a b obj leafPageId
                     leafPage' <- pageGet b leafPageId'
                     doInsert leafPageId' leafPage'
             else doInsert leafPageId leafPage



