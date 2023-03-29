module Main where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF, runFree, runFreeM)
import Control.Monad.ST (ST, run)
import Control.Safely (traverse_)
import Data.Array (head, (..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import IntPlus as IntPlus
import Lan (Lan, lan, unLan)

data TestF f
  = Log String f
  | Feedback (Lan Maybe Array f)
  | Traverse (Array f)

derive instance Functor TestF

type TestM = Free TestF

logT :: String -> TestM Unit
logT s = liftF $ Log s unit

feedbackT :: forall a. Array a -> TestM (Maybe a)
feedbackT a = liftF $ Feedback $ lan a

test :: TestM Unit
test = do
  as <- feedbackT [ 1, 2, 3 ]
  logT $ show as

interpreter :: TestF (Free TestF Unit) -> Effect (Free TestF Unit)
interpreter = case _ of
  Log s next -> log s *> pure next -- log
  Feedback t -> pure $ unLan (\arr callback -> callback $ head arr) t -- return head
  Traverse arr -> pure $ traverse_ identity arr -- traverse

main :: Effect Unit
main = do
  let
    logT = 2
  -- Result:  Just 1
  runFreeM interpreter test
  log $ show value6

main2 :: Effect Int
main2 = foldM
  do
    \acc x -> do
      log $ show x
      pure $ acc + x
  do 0
  do 1 .. 10

x :: forall a. Maybe a
x = Nothing

y :: forall b14 a15. b14 -> Maybe a15
y = const x

infixr 0 identity as $$

mkST :: forall r. Int -> ST r Int
mkST x = pure x

testST :: Int
testST = run do mkST 0

testST2 :: Int
testST2 = run $$ mkST 0

value6 :: Int
value6 = IntPlus.do
  1
  2
  3
