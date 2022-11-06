module Main where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Console (log)

data T f a b = T b (f b -> a)

newtype TE f a = TE (Exists (T f a))

instance Functor (TE f) where
  map f (TE e) = TE $ runExists (\(T b g) -> mkExists $ T b (f <<< g)) e

te :: forall f a. a -> TE f (f a)
te a = TE $ mkExists $ T a identity

unTe :: forall f a. (forall b. b -> f b) -> TE f a -> a
unTe f (TE e) = runExists (\(T b g) -> g $ f b) e

type TEA = TE Array

x :: TEA (Array Int)
x = te 1

y :: TEA Int
y = map sum x

z :: Int
z = unTe (\i -> [ i, i, i ]) y

data TestF f
  = Log String f
  | Feedback (TE Array f)

type TestM = Free TestF

logT :: String -> TestM Unit
logT s = liftF $ Log s unit

feedbackT :: forall a. a -> TestM (Array a)
feedbackT a = liftF $ Feedback $ te a

test :: TestM Unit
test = do
  as <- feedbackT 1
  logT $ show as

interpreter :: TestF ~> Effect
interpreter = case _ of
  Log s next -> log s *> pure next -- log
  Feedback t -> pure $ unTe (\a -> [ a, a, a ]) t -- return array of 3 a's

main :: Effect Unit
main = do
  -- result: [1, 1, 1]
  foldFree interpreter test

  log $ show z
