module WriterTraced where

import Prelude

import Data.Functor.Pairing (type (⋈), zap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

program :: Tuple String Int
program = do
  x <- Tuple "x" $ 10
  y <- Tuple "y" $ 20
  pure $ x + y

interpreter :: String -> Int -> String
interpreter s x = "log: " <> s <> ", result: " <> show x

funcTuple :: forall s. Function s ⋈ Tuple s
funcTuple f g (Tuple s b) = f (g s) b

result :: String
result = zap funcTuple interpreter program

main :: Effect Unit
main = log result
