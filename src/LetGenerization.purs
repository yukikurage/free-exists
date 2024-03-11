module LetGenerization where

import Prelude

x :: forall a. a -> a
x a = a

f :: String
f =
  let
    y = x
  in
    show (y 1) <> y "2"

-- f' :: String
-- f' =
--   let
--     y = (\_ -> x) unit
--   in
--     show (y 1) <> y "2"
