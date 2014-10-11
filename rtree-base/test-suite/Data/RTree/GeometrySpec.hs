{-# LANGUAGE ScopedTypeVariables #-}
module Data.RTree.GeometrySpec where


import           Control.Lens
import           Data.Monoid
import           Data.RTree.Geometry
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Arbitrary

instance Arbitrary Point where
  arbitrary = do
    (a,b) <- arbitrary
    return $ Point a b

instance Arbitrary Rectangle where
  arbitrary = do
    (a,b) <- arbitrary
    return $ mkRectangle a b


spec :: Spec
spec = parallel $ do
  describe "Point" $ do
    prop "x lens returns _x" $ \d -> Point { _x = d, _y = 23} ^. x == d
    prop "y lens returns _y" $ \d -> Point { _x = 23, _y = d} ^. y == d

  describe "Rectangle" $ do
    prop "topLeft lens returns _topLeft" $
      \pa pb -> Rectangle { _topLeft = pa, _bottomRight = pb } ^. topLeft == pa
    prop "bottomRight lens returns _bottomRight" $
      \pa pb -> Rectangle { _topLeft = pa, _bottomRight = pb } ^. bottomRight == pb
    prop "foo" $
      \(r :: Rectangle) ->
       let tl = r ^. topLeft
           br = r ^. bottomRight
       in ((tl ^. x) <= (br ^. x)) && ((tl ^. y) >= (br ^. y))
    prop "mappended to itself does not change " $
      \(ra :: Rectangle) -> ra `mappend` ra == ra
    prop "mappend is relektive" $
      \(ra :: Rectangle)(rb :: Rectangle) -> (mappend ra rb) == (mappend rb ra)
    prop "mappend never results in smaller rectangle " $
      \(ra :: Rectangle)(rb :: Rectangle) ->
       ((rectArea $ mappend ra rb) >= rectArea ra) && ((rectArea $ mappend ra rb) >= rectArea ra)

