module Data.TypeList where

import Prelude
import Prim.Int (class Add)

import Data.Reflectable (class Reflectable)
import Prim.Boolean (False, True)

class TypeEq :: forall k. k -> k -> Boolean -> Constraint
class TypeEq a b c | a b -> c

instance TypeEq a a True
else instance TypeEq a b False

-- | 型レベルリスト
foreign import data TypeList :: Type -> Type

foreign import data TypeNil :: forall k. TypeList k

foreign import data TypeCons :: forall k. k -> TypeList k -> TypeList k

infixr 5 type TypeCons as :+

class FindIndex :: forall k. k -> TypeList k -> Int -> Constraint
class (IndexEq as i a True, Reflectable i Int) <= FindIndex a as i

instance FindIndex a (TypeCons a as) 0
else instance
  ( IndexEq (TypeCons b as) iPlusOne a True
  , FindIndex a as i
  , Add i 1 iPlusOne
  , Reflectable iPlusOne Int
  ) =>
  FindIndex a (TypeCons b as) iPlusOne

class IndexEq :: forall k. TypeList k -> Int -> k -> Boolean -> Constraint
class Reflectable i Int <= IndexEq as i a b

instance IndexEq (TypeCons a as) 0 a True
else instance IndexEq as 0 a False
else instance (IndexEq as i a True, Add i 1 iPlusOne, Reflectable iPlusOne Int) => IndexEq (TypeCons b as) iPlusOne a True

class Member :: forall k. k -> TypeList k -> Constraint
class Member a as

instance Member a (TypeCons a as)
else instance Member a as => Member a (TypeCons b as)

class Append :: forall k. TypeList k -> TypeList k -> TypeList k -> Constraint
class Append as bs cs | as bs -> cs, as cs -> bs, bs cs -> as

instance Append TypeNil bs bs
else instance (Append as bs cs) => Append (TypeCons a as) bs (TypeCons a cs)

test1 :: Unit
test1 = x
  where
  x :: Member Boolean (Int :+ Boolean :+ TypeNil) => Unit
  x = unit

test1' :: Unit
test1' = x
  where
  x :: forall r. Member Boolean (Int :+ Boolean :+ r) => Unit
  x = unit

-- test2Fail :: Unit
-- test2Fail = x
--   where
--   x :: Member String (Int :+ Boolean :+ TypeNil) => Unit
--   x = unit

test3 :: Unit
test3 = x
  where
  x :: FindIndex Boolean (Int :+ Boolean :+ TypeNil) 1 => Unit
  x = unit

test3'' :: Unit
test3'' = x
  where
  x :: forall r. FindIndex Int (Int :+ Boolean :+ Int :+ r) 2 => Unit
  x = unit

-- test4Fail :: Unit
-- test4Fail = x
--   where
--   x :: FindIndex Boolean (Int :+ Boolean :+ TypeNil) 2 => Unit
--   x = unit

test5 :: Unit
test5 = x
  where
  x :: IndexEq (Int :+ Boolean :+ TypeNil) 1 Boolean True => Unit
  x = unit

eqTest :: Unit
eqTest = x
  where
  x :: TypeEq Int Int True => Unit
  x = unit

-- eqTestFail :: Unit
-- eqTestFail = x
--   where
--   x :: TypeEq Int Boolean True => Unit
--   x = unit

eqTest2 :: Unit
eqTest2 = x
  where
  x :: TypeEq Int Boolean False => Unit
  x = unit

-- eqTest2Fail :: Unit
-- eqTest2Fail = x
--   where
--   x :: TypeEq Int Boolean True => Unit
--   x = unit
