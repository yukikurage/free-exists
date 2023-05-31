module Stream where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, deferCofree)
import Control.Monad.Free (Free, liftF)
import Data.Functor.Pairing (freeCofree, sym, zap, type (⋈))
import Data.Functor.Pairing as Pairing
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

type Stream a = Cofree Identity a

iterate :: forall a. (a -> a) -> a -> Stream a
iterate f a = deferCofree \_ -> Tuple a $ Identity $ iterate f (f a)

type Shift = Free Identity

shift :: Shift Unit
shift = liftF $ Identity unit

interpreter :: Stream (Int -> String)
interpreter =
  let
    f :: Int -> Tuple (Int -> String) (Identity Int)
    f sft = Tuple (\x -> "shift: " <> show sft <> ", result: " <> show x) $ Identity $ sft + 1
  in
    buildCofree f 0

program :: Shift Int
program = do
  shift
  shift
  shift
  shift
  pure 10

result :: String
result = zap (sym (freeCofree Pairing.identity)) interpreter program

main :: Effect Unit
main = log result *> log resultT

tupleFunc :: forall s. Tuple s ⋈ Function s
tupleFunc f (Tuple s a) g = f a (g s)

programF :: Int -> Int
programF = do
  x <- (_ + 1)
  y <- (_ * 2)
  pure $ x + y

interpreterT :: Tuple Int (Int -> String)
interpreterT = Tuple 10 \x -> "result: " <> show x

resultT :: String
resultT = zap tupleFunc interpreterT programF
