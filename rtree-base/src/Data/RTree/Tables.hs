{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.RTree.Tables (TabularBackend) where

import           Control.Applicative          hiding (empty)
import           Control.Concurrent.STM.TMVar
import           Control.Lens
import           Control.Monad.STM
import           Data.Monoid
import           Data.RTree.Geometry
import           Data.RTree.Types
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Table


data TabularPage t =
  TabularPage { _tabularId   :: Int
              , _tabularPage :: RTreePage (TabularBackend t) t
              }
newTabularPage = TabularPage 0


newtype TabularBackend t =
  TabularBackend { _tabularTable :: TMVar (Table (TabularPage t)) }


makeLenses ''TabularBackend
makeLenses ''TabularPage

readTable :: TabularBackend t -> STM (Table (TabularPage t))
readTable = readTMVar . view tabularTable

takeTable :: TabularBackend t -> STM (Table (TabularPage t))
takeTable = takeTMVar . view tabularTable

putTable :: TabularBackend t -> Table (TabularPage t) -> STM ()
putTable b = putTMVar $ b ^. tabularTable



instance Tabular (TabularPage t) where
 type PKT (TabularPage t) = Int
 data Key k (TabularPage t) b where
   PageId :: Key Primary (TabularPage t) Int
   PageChildren :: Key Inverted (TabularPage t) (Set (PKT (TabularPage t)))

 data Tab (TabularPage t) i = PageTab (i Primary Int) (i Inverted (Set (PKT (TabularPage t))))

 autoTab = autoIncrement tabularId

 fetch PageId = _tabularId
 fetch PageChildren =  _pageChildren . _tabularPage

 primary = PageId
 primarily PageId i = i

 mkTab f = PageTab <$> f PageId <*> f PageChildren
 forTab (PageTab i s) f = PageTab <$> f PageId i <*> f PageChildren s
 ixTab (PageTab i s) PageId = i
 ixTab (PageTab i s) PageChildren = s


instance (HasRectangle t) => RTreeBackend (TabularBackend t) IO t where
  type RTreePageKey (TabularBackend t) t = PKT (TabularPage t)

  pageInsert b p = atomically $ do
    tbl <- takeTable b
    let (p', t') = insert' (newTabularPage p) tbl
    putTable b t'
    return $ p' ^. tabularId

  pageGet b pid = do
    tbl <- atomically $ readTable b
    maybe (fail "pageGet: unable to find page") return $ getPageTable tbl pid

  pageDelete b pid = atomically $ do
    tbl <- takeTable b
    let tbl' = undefined
    putTable b tbl'
    return True

  pageSetData = pageSet tabularData
  pageSetChildren = pageSet tabularChildren
  pageSetBoundingBox = pageSet tabularBoundingBox

  pageGetParentKey b cId = do
    tbl <- atomically $ readTable b
    let r = tbl ^. withAny PageChildren [cId]
    return $ fmap _tabularId $ r ^? ix 0

getPageTable :: Table (TabularPage t) -> PKT (TabularPage t) -> Maybe (RTreePage (TabularBackend t) t)
getPageTable tbl pid = do
  p <- getPageTable' tbl pid
  return $ p ^. tabularPage

getPageTable' tbl pid =
    let x = tbl ^. (with PageId (==) pid)
    in x ^? ix 0

setTabularValue s tbl pid v =
  case (getPageTable' tbl pid) of
    Nothing -> error "setTabularValue: unable to find page with key."
    Just p -> insert (set s v p) $ deleteWith PageId (==) pid tbl

setTabularChildren = setTabularValue tabularChildren
setTabularData = setTabularValue tabularData
setTabularBoundingBox = setTabularValue tabularBoundingBox

pageSet s b pid v = atomically $ do
    tbl <- takeTable b
    putTable b $ setTabularValue s tbl pid v


tabularChildren p = tabularPage . pageChildren $ p
tabularData p = tabularPage . pageData $ p
tabularBoundingBox p = tabularPage . pageBoundingBox $ p
