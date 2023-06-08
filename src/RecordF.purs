module RecordF where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record.Unsafe as RecordUnsafe
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

{-
in TS;

^^

Map<String, {
  value: any,
  map: (f: (a: any) -> any) -> (value: any) -> any
}>
-}

foreign import data RecordF :: Row (Type -> Type) -> Type -> Type

class RecordFCoercible :: Row Type -> Row (Type -> Type) -> Type -> Constraint
class RecordFCoercible r f a where
  mkRecordF :: Record r -> RecordF f a
  unRecordF :: RecordF f a -> Record r

instance (RowToList f fList, RowToList r rList, RecordFCoercibleWithList rList r fList f a) => RecordFCoercible r f a where
  mkRecordF = mkRecordFInternal (Proxy :: Proxy rList) (Proxy :: Proxy fList)
  unRecordF = unRecordFInternal (Proxy :: Proxy rList) (Proxy :: Proxy fList)

class RecordFCoercibleWithList :: RowList Type -> Row Type -> RowList (Type -> Type) -> Row (Type -> Type) -> Type -> Constraint
class RecordFCoercibleWithList rList r fList f a | rList -> r, fList -> f, rList fList -> a where
  mkRecordFInternal :: Proxy rList -> Proxy fList -> Record r -> RecordF f a
  unRecordFInternal :: Proxy rList -> Proxy fList -> RecordF f a -> Record r

foreign import unsafeEmptyRecordF :: forall r a. RecordF r a

instance RecordFCoercibleWithList RowList.Nil () RowList.Nil () a where
  mkRecordFInternal _ _ _ = unsafeEmptyRecordF
  unRecordFInternal _ _ _ = {}

foreign import unsafeUnRecord :: forall fr a r. RecordF fr a -> Record r

foreign import data Value :: Type

foreign import unsafeConsRecordF :: forall fr fr' a. String -> Value -> ((Value -> Value) -> Value -> Value) -> RecordF fr a -> RecordF fr' a

instance
  ( RecordFCoercibleWithList rList r fList f a
  , Row.Cons label (fHead a) r rNew
  , Row.Cons label fHead f fNew
  , IsSymbol label
  , Functor fHead
  ) =>
  RecordFCoercibleWithList (RowList.Cons label (fHead a) rList) rNew (RowList.Cons label fHead fList) fNew a where
  mkRecordFInternal _ _ record = unsafeConsRecordF (reflectSymbol (Proxy :: Proxy label)) valueInternal mapInternal tailRep
    where
    mapInternal f value = unsafeCoerce (map f (unsafeCoerce value :: fHead Value))
    tailRep = mkRecordFInternal (Proxy :: Proxy rList) (Proxy :: Proxy fList) $ unsafeCoerce $ RecordUnsafe.unsafeDelete (reflectSymbol (Proxy :: Proxy label)) record
    valueInternal = unsafeCoerce $ RecordUnsafe.unsafeGet (reflectSymbol (Proxy :: Proxy label)) record

  -- map を復元する必要が無いので、再帰が必要ない
  unRecordFInternal :: Proxy (RowList.Cons label (fHead a) rList) -> Proxy (RowList.Cons label fHead fList) -> RecordF fNew a -> Record rNew
  unRecordFInternal _ _ recordFRep = unsafeUnRecord recordFRep

foreign import mapImpl :: forall f a b. (a -> b) -> RecordF f a -> RecordF f b

instance Functor (RecordF f) where
  map = mapImpl

foreign import getFImpl :: forall fr f a. String -> RecordF fr a -> f a

getF :: forall label fr fr' f a. IsSymbol label => Row.Cons label f fr' fr => Proxy label -> RecordF fr a -> f a
getF proxy recordF = getFImpl (reflectSymbol proxy) recordF

foreign import setFImpl :: forall fr1 fr2 f a. String -> f a -> ((Value -> Value) -> Value -> Value) -> RecordF fr1 a -> RecordF fr2 a

setF :: forall fr1 fr2 fr label f g a. Functor g => IsSymbol label => Row.Cons label f fr1 fr => Row.Cons label g fr2 fr => Proxy label -> g a -> RecordF fr1 a -> RecordF fr2 a
setF proxy value recordF = setFImpl (reflectSymbol proxy) value mapInternal recordF
  where
  mapInternal f val = unsafeCoerce (map f (unsafeCoerce val :: g Value))

foreign import modifyFImpl :: forall fr1 fr2 f g a. String -> (f a -> g a) -> ((Value -> Value) -> Value -> Value) -> RecordF fr1 a -> RecordF fr2 a

modifyF :: forall fr1 fr2 fr label f g a. Functor g => IsSymbol label => Row.Cons label f fr1 fr => Row.Cons label g fr2 fr => Proxy label -> (f a -> g a) -> RecordF fr1 a -> RecordF fr2 a
modifyF proxy f recordF = modifyFImpl (reflectSymbol proxy) f mapInternal recordF
  where
  mapInternal g val = unsafeCoerce (map g (unsafeCoerce val :: g Value))

foreign import insertFImpl :: forall fr1 fr2 f a. String -> f a -> ((Value -> Value) -> Value -> Value) -> RecordF fr1 a -> RecordF fr2 a

insertF :: forall fr1 fr2 label f a. Functor f => IsSymbol label => Lacks label fr1 => Cons label f fr1 fr2 => Proxy label -> f a -> RecordF fr1 a -> RecordF fr2 a
insertF proxy value recordF = insertFImpl (reflectSymbol proxy) value mapInternal recordF
  where
  mapInternal f val = unsafeCoerce (map f (unsafeCoerce val :: f Value))

foreign import deleteFImpl :: forall fr1 fr2 a. String -> RecordF fr1 a -> RecordF fr2 a

deleteF :: forall rf1 rf2 label f a. IsSymbol label => Lacks label rf1 => Cons label f rf1 rf2 => Proxy label -> RecordF rf2 a -> RecordF rf1 a
deleteF proxy recordF = deleteFImpl (reflectSymbol proxy) recordF

foreign import mergeFImpl :: forall fr1 fr2 fr3 a. RecordF fr1 a -> RecordF fr2 a -> RecordF fr3 a

mergeF :: forall fr1 fr2 fr3 fr4 a. Union fr1 fr2 fr3 => Nub fr3 fr4 => RecordF fr1 a -> RecordF fr2 a -> RecordF fr4 a
mergeF = mergeFImpl

disjointUnionF :: forall fr1 fr2 fr3 a. Union fr1 fr2 fr3 => Nub fr3 fr3 => RecordF fr1 a -> RecordF fr2 a -> RecordF fr3 a
disjointUnionF = mergeFImpl

coon
  :: forall fr1 fr2 label f a b
   . Functor f
  => IsSymbol label
  => Lacks label fr1
  => Cons label f fr1 fr2
  => Proxy label
  -> (b -> f a)
  -> (b -> RecordF fr1 a)
  -> b
  -> RecordF fr2 a
coon proxy f g b = insertF proxy (f b) (g b)

test
  :: Record
       ( a :: Maybe Int
       , b :: Array Int
       , c :: Tuple String Int
       )
test =
  { a: Just 1
  , b: [ 1, 2, 3 ]
  , c: Tuple "hello" 1
  }

testF
  :: RecordF
       ( a :: Maybe
       , b :: Array
       , c :: Tuple String
       )
       Int
testF = mkRecordF test

testF2
  :: RecordF
       ( a :: Maybe
       , b :: Array
       , c :: Tuple String
       )
       String
testF2 = map ((_ + 1) >>> show) testF

test2
  :: Record
       ( a :: Maybe String
       , b :: Array String
       , c :: Tuple String String
       )
test2 = unRecordF testF2

testF3
  :: RecordF
       ( b :: Array
       , c :: Tuple String
       )
       Int
testF3 = deleteF (Proxy :: Proxy "a") testF

testF4
  :: RecordF
       ( a :: Maybe
       , b :: Array
       )
       String
testF4 = deleteF (Proxy :: Proxy "c") testF2

testF5
  :: RecordF
       ( a :: Maybe
       , b :: Array
       , c :: Tuple String
       )
       Int
testF5 = mergeF (map (length) testF4) testF3

test3 :: Record (b :: Array Int, c :: Tuple String Int)
test3 = unRecordF testF3

test4 :: Record (a :: Maybe String, b :: Array String)
test4 = unRecordF testF4

test5 :: Record (a :: Maybe Int, b :: Array Int, c :: Tuple String Int)
test5 = unRecordF testF5

main :: Effect Unit
main = do
  logShow test
  logShow (unRecordF testF :: Record (a :: Maybe Int, b :: Array Int, c :: Tuple String Int))
  logShow test2

  logShow test3
  logShow test4
  logShow test5
