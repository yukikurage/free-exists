module EventCofreeBehaviorFree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Control.Monad.Free (Free, resume, wrap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)

-- | 予想 : Cofree(Behavior) において直積が分解でき、Free(Event) において直和が分解できる

divideCofree :: forall f a b. Functor f => Cofree f (Tuple a b) -> Tuple (Cofree f a) (Cofree f b)
divideCofree cf =
  let
    Tuple hl hr = head cf
    ft = tail cf
    fDividedT = divideCofree <$> ft
    fDividedTL = fst <$> fDividedT
    fDividedTR = snd <$> fDividedT
  in
    Tuple (hl :< fDividedTL) (hr :< fDividedTR)

divideFree :: forall f a b. Functor f => Free f (Tuple a b) -> Tuple (Free f a) (Free f b)
divideFree f = case resume f of
  Left ft ->
    let
      fDividedT = map divideFree ft
      fDividedTL = fst <$> fDividedT
      fDividedTR = snd <$> fDividedT
    in
      Tuple (wrap fDividedTL) (wrap fDividedTR)
  Right (Tuple hl hr) -> Tuple (pure hl) (pure hr)
