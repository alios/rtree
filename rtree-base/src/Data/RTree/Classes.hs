{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.RTree.Classes where

import           Control.Lens
import           Data.Maybe          (fromJust)
import           Data.Monoid
import           Data.RTree.Geometry
import           Data.RTree.Types
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)


class (Ord t, RTreeBackend b m t) => RTreeFrontend a b m t where

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
          pageSetChildren b leafPageId $ Set.insert pId $ leafPage ^. pageChildren
          return pId
    in do leafPageId <- locatePage a b obj $ tr ^. rTreeRootNode
          leafPage <- pageGet b leafPageId
          if ((leafPage ^. pageChildren . to Set.size) >= maxChildren a b)
             then do leafPageId' <- splitPage a b obj leafPageId
                     leafPage' <- pageGet b leafPageId'
                     doInsert leafPageId' leafPage'
             else doInsert leafPageId leafPage

  rTreeQuery :: (HasRectangle bbox) => RTree b m t -> bbox -> m (Set t)
  rTreeQuery tr bbox =
    let b = tr ^. rTreeBackend
        rTreeQuery' p =
          if (not $ p `rectangleIn` bbox)
             then return mempty
             else case (p ^. pageData) of
                    Just obj -> return $ Set.singleton obj
                    Nothing -> do
                      cps <- sequence . fmap (pageGet b) $ Set.toList $ p ^. pageChildren
                      cps' <- sequence . fmap rTreeQuery' $ cps
                      return $ mconcat cps'
    in do
      rp <- pageGet b $ tr ^. rTreeRootNode
      rTreeQuery' rp

  rTreeDelete :: a -> b -> RTree b m t -> RTreePageKey b t -> m ()
  rTreeDelete a b tr pId = do
    leafPageId_ <- pageGetParentKey b pId
    let leafPageId = maybe (error "rTreeDelete: unable to find page") id $ leafPageId_
    leafPage <- pageGet b leafPageId
    let cs = leafPage ^. pageChildren & contains pId .~ False
    csM <- sequence . fmap (pageGet b) $ Set.toList cs
    let bbox = mconcat $ fmap (view rectangle) csM
    pageSetChildren b leafPageId cs
    pageSetBoundingBox b leafPageId bbox
    pageDelete b pId
    if (Set.size cs >= minChildren a b)
      then return ()
      else let csObj = fmap (fromJust . view pageData) csM
           in undefined

  locatePage :: a -> b -> t -> RTreePageKey b t -> m (RTreePageKey b t)
  splitPage ::  a -> b -> t -> RTreePageKey b t-> m (RTreePageKey b t)
  maxChildren :: a -> b -> Int
  minChildren :: a -> b -> Int
