module Como where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, deferCofree, explore)
import Control.Monad.Free (Free, liftF)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

data TestF s a = TestRead (s -> a) | TestWrite s a

derive instance Functor (TestF b)

type TestFree s = Free (TestF s)

readTest :: forall s. TestFree s s
readTest = liftF $ TestRead identity

writeTest :: forall s. s -> TestFree s Unit
writeTest s = liftF $ TestWrite s unit

data TestG s a = TestG s a (s -> a)

derive instance Functor (TestG s)

type TestCofree s = Cofree (TestG s)

cofreeInterpreter :: forall s. s -> TestCofree s s
cofreeInterpreter s = buildCofree (\acc -> Tuple acc $ TestG acc acc identity) s

trans :: forall s x y. TestF s (x -> y) -> TestG s x -> y
trans = case _ of
  TestRead (f :: s -> x -> y) -> \(TestG s x _) -> f s x
  TestWrite s (f :: x -> y) -> \(TestG _ _ (g :: s -> x)) -> f $ g s

program :: TestFree String (String -> String)
program = do
  z <- readTest
  writeTest "Hello"
  x <- readTest
  _ <- readTest
  writeTest "World"
  y <- readTest
  writeTest $ x <> " " <> y <> z
  pure identity

result :: String
result = explore trans program $ cofreeInterpreter "!"

main :: Effect Unit
main = log result
