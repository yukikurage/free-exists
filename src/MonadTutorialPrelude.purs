module MonadTutorialPrelude where

import Prelude

program3 :: Array Int
program3 = do
  x <- [ 0, 1 ]
  y <- [ x, x + 1, x + 2 ]
  [ x + y ]
