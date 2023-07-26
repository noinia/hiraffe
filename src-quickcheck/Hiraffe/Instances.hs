{-# OPTIONS_GHC -Wno-orphans #-}
module Hiraffe.Instances
  () where

import Hiraffe.PlanarGraph.Dart as Dart
import Test.QuickCheck (Arbitrary(..), suchThat)

--------------------------------------------------------------------------------

instance Arbitrary (Dart.Arc s) where
  arbitrary = Dart.Arc <$> (arbitrary `suchThat` (>= 0))

instance Arbitrary Dart.Direction where
  arbitrary = (\b -> if b then Dart.Positive else Dart.Negative) <$> arbitrary

instance Arbitrary (Dart.Dart s) where
  arbitrary = Dart.Dart <$> arbitrary <*> arbitrary
