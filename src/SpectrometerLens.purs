module SpectrometerLens where

import Prelude

import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Lens (iso, review, toListOf, zipFWithOf, (%~))
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Generics (genericSpectrometer)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

type HelloWorld a = { hello :: a, world :: a }

helloWorldSpectrometer :: forall a b. Spectrometer (HelloWorld a) (HelloWorld b) a b
helloWorldSpectrometer = spectrometer >>> iso
  do \{ hello, world } -> hello /\ world /\ unit
  do \(hello /\ world /\ _) -> { hello, world }

-- | As Review
revTest :: HelloWorld Int
revTest = review helloWorldSpectrometer 1

-- | As Setter
setTest :: HelloWorld Int
setTest = (helloWorldSpectrometer %~ (_ + 1)) revTest

-- | As Zipper
zipTest :: HelloWorld Int
zipTest = zipFWithOf helloWorldSpectrometer sum [ revTest, revTest ]

-- | As Fold
foldTest :: List Int
foldTest = toListOf helloWorldSpectrometer revTest

-- | GenericTest
data GenericTest a = GenericTest a a a a a

derive instance Generic (GenericTest a) _

instance Show a => Show (GenericTest a) where
  show = genericShow

genericTestSpectrometer :: forall a b. Spectrometer (GenericTest a) (GenericTest b) a b
genericTestSpectrometer = genericSpectrometer

-- | As Review
-- | (GenericTest 2 2 2 2 2)
revGenericTest :: GenericTest Int
revGenericTest = review genericTestSpectrometer 2

-- | As Setter
-- | (GenericTest 3 3 3 3 3)
setGenericTest :: GenericTest Int
setGenericTest = (genericTestSpectrometer %~ (_ + 1)) revGenericTest

-- | As Zipper
-- | (GenericTest 4 4 4 4 4)
zipGenericTest :: GenericTest Int
zipGenericTest = zipFWithOf genericTestSpectrometer sum [ revGenericTest, revGenericTest ]

-- | As Fold
-- | (2 : 2 : 2 : 2 : 2 : Nil)
foldGenericTest :: List Int
foldGenericTest = toListOf genericTestSpectrometer revGenericTest
