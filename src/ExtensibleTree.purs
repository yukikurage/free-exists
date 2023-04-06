module ExtensibleTree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Data.Foldable (class Foldable)
import Data.Functor.Variant (class VariantFShows, VariantF, case_, inj, on)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable)
import Data.Variant.Internal (class VariantTags)
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type ExTree r a = Cofree (VariantF r) a

type ExForest r a = VariantF r (ExTree r a)

mkTree :: forall r a. a -> ExForest r a -> ExTree r a
mkTree = mkCofree

newtype StringForShow = StringForShow String

instance Show StringForShow where
  show (StringForShow s) = s

derive instance Newtype StringForShow _

drawTree :: forall r rl. RowToList r rl => VariantTags rl => VariantFShows rl StringForShow => ExTree r String -> String
drawTree = unwrap <<< internal
  where
  internal :: ExTree r String -> StringForShow
  internal et = wrap $ head et <> "(" <> show (map internal $ tail et) <> ")"

showTree :: forall r rl a. RowToList r rl => VariantTags rl => VariantFShows rl StringForShow => Show a => ExTree r a -> String
showTree = drawTree <<< map show

scanTree :: forall r a b. (a -> b -> b) -> b -> ExTree r a -> ExTree r b
scanTree f b et = f (head et) b :< map (scanTree f b) (tail et)

setNodeValue :: forall r a. a -> ExTree r a -> ExTree r a
setNodeValue a et = a :< tail et

modifyNodeValue :: forall r a. (a -> a) -> ExTree r a -> ExTree r a
modifyNodeValue f et = f (head et) :< tail et

foldTree :: forall r a b. (a -> VariantF r b -> b) -> ExTree r a -> b
foldTree f et = f (head et) $ map (foldTree f) $ tail et

foldTreeWithDepth :: forall r a b. (a -> Int -> VariantF r b -> b) -> Int -> ExTree r a -> b
foldTreeWithDepth f d et = f (head et) d $ map (foldTreeWithDepth f (d + 1)) $ tail et

----------
-- TEST --
----------

data AddExp a = AddExp a a

derive instance Functor AddExp
derive instance Foldable AddExp
derive instance Traversable AddExp
instance Show a => Show (AddExp a) where
  show (AddExp a b) = "(" <> show a <> " + " <> show b <> ")"

type ADD r = (add :: AddExp | r)

_add :: Proxy "add"
_add = Proxy

addE :: forall r. ExTree (ADD + r) Unit -> ExTree (ADD + r) Unit -> ExTree (ADD + r) Unit
addE a b = mkTree unit $ inj _add (AddExp a b)

data MulExp a = MulExp a a

derive instance Functor MulExp
derive instance Foldable MulExp
derive instance Traversable MulExp
instance Show a => Show (MulExp a) where
  show (MulExp a b) = "(" <> show a <> " * " <> show b <> ")"

type MUL r = (mul :: MulExp | r)

_mul :: Proxy "mul"
_mul = Proxy

mulE :: forall r. ExTree (MUL + r) Unit -> ExTree (MUL + r) Unit -> ExTree (MUL + r) Unit
mulE a b = mkTree unit $ inj _mul (MulExp a b)

data ValExp :: Type -> Type -> Type
data ValExp v a = ValExp v

derive instance Functor (ValExp v)
derive instance Foldable (ValExp v)
derive instance Traversable (ValExp v)
instance Show v => Show (ValExp v a) where
  show (ValExp a) = show a

type VAL v r = (val :: ValExp v | r)

_val :: Proxy "val"
_val = Proxy

valE :: forall v r. v -> ExTree (VAL v + r) Unit
valE v = mkTree unit $ inj _val (ValExp v)

type Exp v = ExTree (ADD + MUL + VAL v + ()) Unit

eval :: forall v. Semiring v => Exp v -> v
eval = foldTree $ \_ -> f
  where
  f :: VariantF (ADD + MUL + VAL v + ()) v -> v
  f = case_
    # on _add (\(AddExp a b) -> a + b)
    # on _mul (\(MulExp a b) -> a * b)
    # on _val (\(ValExp v) -> v)

-- | 1 + 2 * 3
test :: Exp Int
test = addE (valE 1) (mulE (valE 2) (valE 3))

main :: Effect Unit
main = do
  log $ showTree test
  log $ show $ eval test
