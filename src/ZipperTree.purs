module ZipperTree where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))

newtype Zipper a = Zipper (Tree a /\ List (Crumb a))

derive instance Functor Zipper

data Tree a = Node a (Tree a) (Tree a) | Leaf a

derive instance Functor Tree

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)

derive instance Functor Crumb

childR :: forall a. Zipper a -> Maybe (Zipper a)
childR (Zipper (Node x l r /\ crumbs)) = Just $ Zipper (r /\ RightCrumb x l : crumbs)
childR _ = Nothing

childL :: forall a. Zipper a -> Maybe (Zipper a)
childL (Zipper (Node x l r /\ crumbs)) = Just $ Zipper (l /\ LeftCrumb x r : crumbs)
childL _ = Nothing

root :: forall a. Zipper a -> Maybe (Zipper a)
root (Zipper (t /\ LeftCrumb x r : crumbs)) = Just $ Zipper (Node x t r /\ crumbs)
root (Zipper (t /\ RightCrumb x l : crumbs)) = Just $ Zipper (Node x l t /\ crumbs)
root _ = Nothing

instance Extend Zipper where
  extend :: forall a b. (Zipper a -> b) -> Zipper a -> Zipper b
  extend =
    let
      dup :: Zipper a -> Zipper (Zipper a)
      dup zipper = Zipper (dupTree zipper /\ dupCrumbs zipper)
        where
        dupTree :: Zipper a -> Tree (Zipper a)
        dupTree (Zipper (Node x l r /\ crumbs)) =
          let
            leftZ = Zipper (l /\ LeftCrumb x r : crumbs)
            rightZ = Zipper (r /\ RightCrumb x l : crumbs)
          in
            Node (Zipper (Node x l r /\ crumbs)) (dupTree leftZ) (dupTree rightZ)
        dupTree _ = Leaf zipper

        dupCrumbs :: Zipper a -> List (Crumb (Zipper a))
        dupCrumbs (Zipper (t /\ LeftCrumb x r : crumbs)) =
          let
            rootZ = Zipper (Node x t r /\ crumbs)
            rightZ = Zipper (r /\ RightCrumb x t : crumbs)
          in
            LeftCrumb rootZ (dupTree rightZ) : dupCrumbs rootZ
        dupCrumbs (Zipper (t /\ RightCrumb x l : crumbs)) =
          let
            rootZ = Zipper (Node x l t /\ crumbs)
            leftZ = Zipper (l /\ LeftCrumb x t : crumbs)
          in
            RightCrumb rootZ (dupTree leftZ) : dupCrumbs rootZ
        dupCrumbs _ = Nil
    in
      \f zip -> map f $ dup zip

instance Comonad Zipper where
  extract :: forall a. Zipper a -> a
  extract (Zipper (Node x _ _ /\ _)) = x
  extract (Zipper (Leaf x /\ _)) = x
