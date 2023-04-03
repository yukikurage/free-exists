module ExtensibleTree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail)
import Data.Foldable (class Foldable)
import Data.Functor.Variant (class FoldableVFRL, class TraversableVFRL, class VariantFShows, VariantF, case_, inj, on)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable)
import Data.Variant.Internal (class VariantTags)
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype ExTree r a = ExTree (Cofree (VariantF r) a)

derive instance Newtype (ExTree r a) _
derive instance Functor (ExTree r)
derive newtype instance (RowToList row rl, FoldableVFRL rl row) => Foldable (ExTree row)
derive newtype instance (RowToList row rl, TraversableVFRL rl row) => Traversable (ExTree row)
instance (RowToList r rl, VariantTags rl, VariantFShows rl (ExTree r a), Show a) => Show (ExTree r a) where
  show et = show (nodeValue et) <> "(" <> show (children et) <> ")"

type ExForest r a = VariantF r (ExTree r a)

mkTree :: forall r a. a -> ExForest r a -> ExTree r a
mkTree a forest = ExTree $ mkCofree a $ map unwrap forest

nodeValue :: forall r a. ExTree r a -> a
nodeValue (ExTree et) = head et

children :: forall r a. ExTree r a -> ExForest r a
children (ExTree et) = map wrap $ tail et

-- drawTree :: forall r rl. RowToList r rl => VariantTags rl => VariantFShows rl String => ExTree r String -> String
-- drawTree et = nodeValue et <> "(" <> fold (map drawTree $ children et) <> ")"

-- showTree :: forall r rl a. RowToList r rl => VariantTags rl => VariantFShows rl String => Show a => ExTree r a -> String
-- showTree = drawTree <<< map show

scanTree :: forall r a b. (a -> b -> b) -> b -> ExTree r a -> ExTree r b
scanTree f b et = mkTree (f (nodeValue et) b) $ map (scanTree f b) (children et)

setNodeValue :: forall r a. a -> ExTree r a -> ExTree r a
setNodeValue a et = mkTree a $ children et

modifyNodeValue :: forall r a. (a -> a) -> ExTree r a -> ExTree r a
modifyNodeValue f et = mkTree (f (nodeValue et)) $ children et

foldTree :: forall r a b. (a -> VariantF r b -> b) -> ExTree r a -> b
foldTree f et = f (nodeValue et) $ map (foldTree f) $ children et

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

data MulExp a = MulExp a a

derive instance Functor MulExp
derive instance Foldable MulExp
derive instance Traversable MulExp
instance Show a => Show (MulExp a) where
  show (MulExp a b) = "(" <> show a <> " * " <> show b <> ")"

type MUL r = (mul :: MulExp | r)

_mul :: Proxy "mul"
_mul = Proxy

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
test = mkTree unit $ inj _add
  ( AddExp
      (mkTree unit $ inj _val (ValExp 1))
      ( mkTree unit $ inj _mul
          ( MulExp
              (mkTree unit $ inj _val (ValExp 2))
              (mkTree unit $ inj _val (ValExp 3))
          )
      )
  )

main :: Effect Unit
main = do
  log $ show test
  log $ show $ eval test
