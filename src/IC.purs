module IC where

import Prelude

import Control.Alternative (class Alternative, empty, guard)
import Data.List (List(..), fromFoldable, length, reverse, (:), uncons)
import Data.Maybe (Maybe, maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (fromMaybe)

tails :: forall a. List a -> List (List a)
tails = reverse <<< loop Nil
  where
  loop :: List (List a) -> List a -> List (List a)
  loop z Nil = z
  loop z xs@(_ : xs') = loop (xs : z) xs'

guardWith :: forall f a. Alternative f => Maybe a -> f a
guardWith = maybe empty pure

inversionCount :: forall a. Ord a => List a -> Int
inversionCount xs = length do
  { head: x, tail: ys } <- (fromMaybe <<< uncons) =<< tails xs
  { head: y, tail: _ } <- fromMaybe <<< uncons =<< tails ys
  guard $ x > y
  pure $ x /\ y

test :: Int
test =
  inversionCount testList

testList :: List Int
testList =
  fromFoldable [ 1, 3, 5, 2, 4, 6 ]
