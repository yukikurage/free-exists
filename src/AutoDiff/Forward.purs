module AutoDiff.Forward where

import Prelude

import AutoDiff.Smooth (SMOOTH, Smooth(..), _smooth, c, der1, der2L, der2R, op0, op1, op2, (:*), (:+))
import Prim.Row (class Union)
import Run (Run, expand, interpret, on, send)
import Type.Row (type (+))

data Dual x = Dual x x

dualM :: forall m x. Monad m => m x -> m x -> m (Dual x)
dualM f f' = do
  x <- f
  x' <- f'
  pure $ Dual x x'

v :: forall x. Dual x -> x
v (Dual x _) = x

dv :: forall x. Dual x -> x
dv (Dual _ x) = x

diff :: forall r x. Union r (SMOOTH x + ()) (SMOOTH x + r) => Run (SMOOTH (Dual x) + r) ~> Run (SMOOTH x + r)
diff = interpret (on _smooth handler (send >>> expand))
  where
  handler :: Smooth (Dual x) ~> Run (SMOOTH x + r)
  handler = case _ of
    Ap0 n k -> do
      r <- dualM (op0 n) (c 0)
      pure $ k r
    Ap1 u (Dual x dx) k -> do
      r <- dualM (op1 u x) (der1 u (pure x) :* pure dx)
      pure $ k r
    Ap2 b (Dual x dx) (Dual y dy) k -> do
      r <- dualM (op2 b x y) (der2L b (pure x) (pure y) :* pure dx :+ der2R b (pure x) (pure y) :* pure dy)
      pure $ k r

d :: forall x. (Dual x -> Run (SMOOTH (Dual x) + ()) (Dual x)) -> x -> Run (SMOOTH x + ()) x
d f x = do
  cm <- c 1
  diffDual <- diff $ f $ Dual x cm
  pure $ dv diffDual
