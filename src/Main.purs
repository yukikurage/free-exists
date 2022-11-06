module Main where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect)
import Effect.Console (log)

data FeedbackE f a = FeedbackE a (Array a -> f)

newtype FeedbackF f = FeedbackF (Exists (FeedbackE f))

instance Functor FeedbackF where
  map f (FeedbackF e) = FeedbackF $ runExists (\(FeedbackE a g) -> mkExists $ FeedbackE a (f <<< g)) e

data TestF f
  = Log String f
  | Feedback (FeedbackF f)

derive instance Functor TestF

type TestM = Free TestF

logT :: String -> TestM Unit
logT s = liftF $ Log s unit

feedbackT :: forall a. a -> TestM (Array a)
feedbackT a = liftF $ Feedback $ FeedbackF $ mkExists $ FeedbackE a identity

test :: TestM Unit
test = do
  as <- feedbackT 1
  logT $ show as

interpreter :: TestF ~> Effect
interpreter = case _ of
  Log s next -> log s *> pure next -- log
  Feedback (FeedbackF e) -> runExists (\(FeedbackE a g) -> pure $ g [ a, a, a ]) e -- return array of 3 a's

main :: Effect Unit
main = do
  -- result: [1, 1, 1]
  runFreeM interpreter test
