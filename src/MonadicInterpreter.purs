module MonadicInterpreter where

import Prelude

import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)

monadicInterpreter :: forall m a. Monad m => Free m a -> m a
monadicInterpreter = resume >>> case _ of
  Right a -> pure a
  Left f -> f >>= monadicInterpreter

-- | Test
type Test = Free Array Int

test :: Test
test = do
  a <- liftF [ 0, 1, 2 ]
  b <- liftF [ 3, 4, 5 ]
  pure (a + b)

main :: Effect Unit
main = do
  logShow $ monadicInterpreter test
