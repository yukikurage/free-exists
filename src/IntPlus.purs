module IntPlus where

import Prelude

discard :: Int -> (Unit -> Int) -> Int
discard a b = a + b unit
