-- | Monads from comonads,
-- | based on <https://hackage.haskell.org/package/kan-extensions-5.0.1/docs/Control-Monad-Co.html>.

module Data.Functor.Pairing.Co where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Class (class ComonadAsk, class ComonadEnv, ask, local)
import Control.Comonad.Store.Class (class ComonadStore, peek, pos)
import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Functor.Pairing (type (⋈))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

newtype Co r w a = Co (w (a -> r) -> r)

co :: forall r w a. (w (a -> r) -> r) -> Co r w a
co = Co

runCo :: forall w a r. Co r w a -> w (a -> r) -> r
runCo (Co cow) = cow

{-
Pairing

(a -> b -> c) -> f a -> g b -> c
-}

newtype ForallCo w a = ForallCo (forall r. Co r w a)

forallCo :: forall w a. (forall r. Co r w a) -> ForallCo w a
forallCo = ForallCo

unForallCo :: forall w a. ForallCo w a -> forall r. Co r w a
unForallCo (ForallCo f) = f

-- | `w` pairs with `Co w` whenever `w` is a `Functor`.
pairCo :: forall w. Functor w => w ⋈ (ForallCo w)
pairCo f w cow = runCo (unForallCo cow) (map f w)

liftCo :: forall r w s. Comonad w => (forall a. w a -> s) -> Co r w s
liftCo f = Co (extract <*> f)

lowerCo :: forall w a s. Functor w => Co (Identity s) w s -> w a -> s
lowerCo m = unwrap <<< runCo m <<< (Identity <$ _)

instance functorCo :: Functor w => Functor (Co r w) where
  map f (Co cow) = Co \w -> cow (map (_ <<< f) w)

instance applyCo :: Extend w => Apply (Co r w) where
  apply (Co f) (Co a) = Co \w -> f (w =>> \wf g -> a (map (_ <<< g) wf))

instance applicativeCo :: Comonad w => Applicative (Co r w) where
  pure a = Co \w -> extract w a

instance bindCo :: Extend w => Bind (Co r w) where
  bind (Co k) f = Co \w -> k (w =>> \wa a -> runCo (f a) wa)

instance monadCo :: Comonad w => Monad (Co r w)

instance monadAskCo :: ComonadAsk e w => MonadAsk e (Co r w) where
  ask = liftCo (ask :: forall a. w a -> e)

instance monadReaderCo :: ComonadEnv e w => MonadReader e (Co r w) where
  local f (Co x) = Co (x <<< local f)

instance monadStateCo :: ComonadStore s w => MonadState s (Co r w) where
  state f = do
    s <- liftCo pos
    case f s of
      Tuple a s1 -> Co \w -> peek s1 w a

instance monadTellCo :: (Semigroup t, ComonadTraced t w) => MonadTell t (Co r w) where
  tell t = Co \w -> track t w unit
