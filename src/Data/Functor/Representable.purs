module Data.Functor.Representable where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Distributive (class Distributive)
import Data.FastVect.FastVect as FV
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Pair (Pair(..), fst, snd)
import Data.Reflectable (class Reflectable)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Type.Proxy (Proxy(..))

type Key f = forall a. f a -> a

class Distributive f <= Representable f where
  tabulate :: forall a. (Key f -> a) -> f a

index :: forall f a. f a -> Key f -> a
index fa f = f fa

instance Representable Identity where
  tabulate dict = Identity $ dict unwrap

instance Representable (Function e) where
  tabulate dict e = dict \g -> g e

instance Representable Pair where
  tabulate dict = Pair (dict fst) (dict snd)

instance (Reflectable n Int, Compare n (-1) GT) => Representable (FV.Vect n) where
  tabulate dict = FV.generate (Proxy :: _ n) (\proxy -> dict \vec -> FV.index proxy vec)

instance Representable m => Representable (ReaderT e m) where
  tabulate dict = ReaderT \e -> let dictM keyM = dict (\reader -> index (runReaderT reader e) keyM) in tabulate dictM

-- | そもそも Cofree が Distributive で無い……
-- instance Representable f => Representable (Cofree f) where
--   tabulate :: forall a. (Key (Cofree f) -> a) -> Cofree f a
--   tabulate dict = deferCofree maker
--     where
--     maker :: Unit -> Tuple a (f (Cofree f a))
--     maker _ = Tuple dictHead $ map tabulate $ dictTail

--     -- | dict 内の 1 段進んだ部分まで読む
--     -- | 残りの dict を f にくるんで返す
--     dictTail :: f (Key (Cofree f) -> a)
--     dictTail = tabulate dictF

--     dictF :: Key f -> Key (Cofree f) -> a
--     dictF keyF keyCofree = dict keyCofree

--     -- | dict 内で head が差している部分を取得
--     dictHead :: a
--     dictHead = dict head
