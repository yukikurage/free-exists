module DuplicatedSymbol where

import Prelude
import Prim.Int

import AutoDiff.Smooth (c)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Record (insert)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

-- | Error
-- test1 :: { a :: Int, a :: String }
-- test1 = { a: 1 }

test2 :: { a :: Int, a :: String } -> Int
test2 x = x.a

-- | Error
-- test3 :: { a :: Int, a :: String } -> String
-- test3 x = x.a

test4 :: forall t2 t3. { a :: t2 | t3 } -> t2
test4 x = x.a

-- test5 :: forall t2 t3 t4. Cons "a" t2 t3 t4 => Record t4 -> t2
-- test5 x = x.a

-- call2 :: forall i. Add 1 1 i => Proxy i
-- call2 = Proxy @2

class C a b | a -> b

instance C Int String

-- call3 :: forall b. C Int b => Proxy b
-- call3 = Proxy @"String"

-- call :: forall r label. IsSymbol label => ListToRow (RL.Cons "aaa" Int RL.Nil) r => Record r
-- call = insert (Proxy @"aaa") 1 {}

call4 :: forall r label. IsSymbol label => Cons label Int () r => Record r
call4 = insert (Proxy @label) 1 {}

-- call :: forall r. ListToRow (RL.Cons "aaa" Int RL.Nil) r => Record r
-- call = insert (Proxy @"aaa") 1 {}

-- call2 :: forall x. TypeEquals x Int => x
-- call2 = 1

-- 導出可能？
-- call5 :: forall r label. TypeEquals label "hoge" => IsSymbol label => ListToRow (RL.Cons "hoge" Int RL.Nil) r => Unit
-- call5 = x
--   where
--     x :: Cons label Int () r => Unit
--     x = unit
