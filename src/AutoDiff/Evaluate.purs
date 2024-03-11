module AutoDiff.Evaluate where

import Prelude

import AutoDiff.Smooth (Binary(..), Nullary(..), SMOOTH, Smooth(..), Unary(..), _smooth)
import Run (Run, interpret, on, send)
import Type.Row (type (+))

evaluate :: forall r. Run (SMOOTH Int + r) ~> Run r
evaluate = interpret (on _smooth handler send)
  where
  handler :: Smooth Int ~> Run r
  handler = case _ of
    Ap0 (ConstE i) k -> pure $ k i
    Ap1 NegE x k -> pure $ k (-x)
    Ap2 AddE x y k -> pure $ k (x + y)
    Ap2 MulE x y k -> pure $ k (x * y)
