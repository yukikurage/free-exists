module Evolve where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate, extract)
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail, (:<))
import Data.Array (uncons)
import Data.Functor.Pairing (freeCofree)
import Data.Functor.Variant (VariantF)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Prim.Row (class Cons, class Lacks)
import RecordF (RecordF, coon, getF, mkRecordF, unRecordF)
import Run (Run(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype Evolve r a = Evolve (Cofree (RecordF r) a)

derive newtype instance Functor (Evolve r)
derive newtype instance Extend (Evolve r)
derive newtype instance Comonad (Evolve r)

buildEvolve :: forall s r a. (s -> Tuple a (RecordF r s)) -> s -> Evolve r a
buildEvolve f a = Evolve $ buildCofree f a

evolve :: forall w r. Comonad w => (w ~> RecordF r) -> w ~> Evolve r
evolve f w = Evolve $ buildCofree (\s -> Tuple (extract s) $ f (duplicate s)) w

inject :: forall a. a -> Evolve () a
inject a = Evolve $ a :< mkRecordF {}

receive :: forall r. Evolve r ~> RecordF r
receive (Evolve cf) = map head $ tail cf

divide :: forall r a. Evolve r a -> Tuple a (RecordF r (Evolve r a))
divide (Evolve cf) = Tuple (head cf) $ map Evolve $ tail cf

runEvolve
  :: forall rm rw a b c
   . (forall x y z. (x -> y -> z) -> VariantF rm x -> RecordF rw y -> z)
  -> (a -> b -> c)
  -> Run rm a
  -> Evolve rw b
  -> c
runEvolve pairing f (Run fr) (Evolve cf) = freeCofree pairing f fr cf

evalEvolve
  :: forall r a
   . Array (RecordF r (Evolve r a) -> Evolve r a)
  -> Evolve r a
  -> a
evalEvolve steps ev = case uncons steps of
  Nothing -> fst $ divide ev
  Just { head: h, tail: t } -> evalEvolve t $ h (snd $ divide ev)

{-
Evolve r1 ~> f が
いい感じに

Evolve r1 ~> RecordF r2
に拡張できるといいよね

receive を使えばできそう

insertF を使う？
-}

-- Example
-- (Evolve r1 ~> f) -> Evolve r1 ~> RecordF r2
toEvolveFn
  :: forall fr1 fr2 label f a
   . Functor f
  => IsSymbol label
  => Lacks label fr1
  => Cons label f fr1 fr2
  => Proxy label
  -> (Evolve fr1 a -> f a)
  -> Evolve fr1 a
  -> RecordF fr2 a
toEvolveFn proxy f = coon proxy f receive

{-

EXAMPLE

-}

-- Stream を作る

_stepper :: Proxy "stepper"
_stepper = Proxy

type STEPPER r = (stepper :: Identity | r)

someStream :: Evolve (STEPPER + ()) Int
someStream = buildEvolve
  ( \s -> Tuple s $ mkRecordF
      { stepper: Identity $ s + 1
      }
  )
  0

idxStream :: forall a. Evolve (STEPPER + ()) a -> Int -> a
idxStream ev n
  | n <= 0 = h
      where
      Tuple h _ = divide ev
  | otherwise = idxStream tl (n - 1)
      where
      Tuple _ t = divide ev
      Identity tl = getF _stepper t

-- 新しく Receiver も作る

_receiver :: Proxy "receiver"
_receiver = Proxy

type RECEIVER s r = (receiver :: Function s | r)

data Event = Step Int

someMooreMaker :: forall a. Evolve (STEPPER + ()) a -> Event -> a
someMooreMaker ev (Step i) = idxStream ev i

someEvolve :: Evolve (STEPPER + RECEIVER Event + ()) Int
someEvolve = evolve (coon _receiver someMooreMaker receive) someStream

mainSh :: Effect Unit
mainSh = do
  let
    steps =
      ( map
          ( \f ->
              let
                g x = f
                  ( unRecordF x
                      :: { receiver :: Event -> _
                         , stepper :: Identity _
                         }
                  )
              in
                g
          )
          [ \{ stepper: Identity x } -> x -- +1
          , \{ receiver } -> receiver $ Step 3 -- +3
          , \{ stepper: Identity x } -> x -- +1
          , \{ receiver } -> receiver $ Step 5 -- +5
          ]
      )
  logShow $ evalEvolve steps someEvolve -- 10
  let
    steps2 =
      ( map
          ( \f ->
              let
                g x = f
                  ( unRecordF x
                      :: { receiver :: Event -> _
                         , stepper :: Identity _
                         }
                  )
              in
                g
          )
          [ \{ receiver } -> receiver $ Step 3 -- +3
          , \{ receiver } -> receiver $ Step 5 -- +5
          ]
      )
  logShow $ evalEvolve steps2 someEvolve -- 8
