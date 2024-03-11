module AutoDiff.Smooth where

import Prelude

import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Nullary = ConstE Int
data Unary = NegE
data Binary = AddE | MulE

data Smooth x a
  = Ap0 Nullary (x -> a)
  | Ap1 Unary x (x -> a)
  | Ap2 Binary x x (x -> a)

derive instance Functor (Smooth x)

_smooth :: Proxy "smooth"
_smooth = Proxy

type SMOOTH x r = (smooth :: Smooth x | r)

op0 :: forall r x. Nullary -> Run (SMOOTH x + r) x
op0 n = lift _smooth $ Ap0 n identity

op1 :: forall r x. Unary -> x -> Run (SMOOTH x + r) x
op1 u x = lift _smooth $ Ap1 u x identity

op2 :: forall r x. Binary -> x -> x -> Run (SMOOTH x + r) x
op2 b x y = lift _smooth $ Ap2 b x y identity

c :: forall r x. Int -> Run (SMOOTH x + r) x
c i = op0 $ ConstE i

n :: forall r x. Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
n rx = do
  x <- rx
  op1 NegE x

p :: forall r x. Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
p rx ry = do
  x <- rx
  y <- ry
  op2 AddE x y

infixl 7 p as :+

m :: forall r x. Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
m rx ry = do
  x <- rx
  y <- ry
  op2 MulE x y

infixl 8 m as :*

der1 :: forall r x. Unary -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
der1 NegE _ = n $ c 1

der2L :: forall r x. Binary -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
der2L AddE _ _ = c 1
der2L MulE _ y = y

der2R :: forall r x. Binary -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x -> Run (SMOOTH x + r) x
der2R AddE _ _ = c 1
der2R MulE x _ = x
