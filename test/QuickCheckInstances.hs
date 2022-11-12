module QuickCheckInstances
  (

  ) where

import Hiraffe.PlanarGraph
import Test.QuickCheck (Arbitrary(..),suchThat)

instance Arbitrary (Arc s) where
  arbitrary = Arc <$> (arbitrary `suchThat` (>= 0))

instance Arbitrary Direction where
  arbitrary = (\b -> if b then Positive else Negative) <$> arbitrary

instance Arbitrary (Dart s) where
  arbitrary = Dart <$> arbitrary <*> arbitrary
