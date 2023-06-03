module SrcTree where

import Prelude

import Control.Alt (class Alt)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map

fromPath :: List String -> SrcTree Unit
fromPath = case _ of
  Nil -> unit :< mempty
  Cons x xs -> unit :< Map.singleton x (fromPath xs)

-- fromPaths :: List (List String) -> SrcTree Unit
-- fromPaths =
