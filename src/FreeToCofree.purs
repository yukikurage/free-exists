module FreeToCofree where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree)
import Control.Monad.Free (Free, liftF)
import Data.Functor.Pairing (Pairing, freeCofree)
import Data.Functor.Pairing.Co (Co, co, pairCo)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

type CofreeCo f a = Cofree (Co f) a

pairFreeCofreeCo :: forall f. Functor f => Pairing (Free f) (CofreeCo f)
pairFreeCofreeCo = freeCofree pairCo

buildCofreeCo :: forall f s a. Functor f => (s -> Tuple a (Co f s)) -> s -> CofreeCo f a
buildCofreeCo = buildCofree

-- | 操作を定義する
data F k r a
  = FWrite k r a
  | FRead k (Maybe r -> a)
  | FDelete k a

derive instance Functor (F k r)

type FreeF k r a = Free (F k r) a

-- | それぞれの操作を実行する関数を作る
write :: forall k r. k -> r -> Free (F k r) Unit
write k r = liftF $ FWrite k r unit

read :: forall k r. k -> Free (F k r) (Maybe r)
read k = liftF $ FRead k identity

delete :: forall k r. k -> Free (F k r) Unit
delete k = liftF $ FDelete k unit

-- | Cofree を定義する
type CofreeCoF k r a = CofreeCo (F k r) a

-- | Cofree Interpreter
interpreter :: forall k r. Ord k => CofreeCoF k r Unit
interpreter = buildCofreeCo f Map.empty
  where
  f :: Map k r -> Tuple Unit (Co (F k r) (Map k r))
  f m = Tuple unit $ co case _ of
    FWrite k r g -> g $ Map.insert k r m
    FRead k g -> g (Map.lookup k m) m
    FDelete k g -> g $ Map.delete k m
