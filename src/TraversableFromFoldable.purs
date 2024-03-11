module TraversableFromFoldable where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.List (List(..), (:))

toList :: forall t a. Foldable t => t a -> List a
toList = foldr (:) Nil
