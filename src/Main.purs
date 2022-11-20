module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Array (filter, head, (..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)

data LanHelper :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
data LanHelper g h a b = LanHelper (g b -> a) (h b)

-- | GADTs
-- | Lan :: (g b -> a) -> h b -> Lan g h a
newtype Lan g h a = Lan (Exists (LanHelper g h a))

instance Functor (Lan g h) where
  map f (Lan e) = Lan $ runExists (\(LanHelper g h) -> mkExists $ LanHelper (f <<< g) h) e

lan :: forall g h a. h a -> Lan g h (g a)
lan h = Lan $ mkExists $ LanHelper identity h

unLan :: forall g h a. (h ~> g) -> Lan g h a -> a
unLan f (Lan e) = runExists (\(LanHelper g b) -> g $ f b) e

data TestF f
  = Log String f
  | Feedback (Lan Maybe Array f)

type TestM = Free TestF

logT :: String -> TestM Unit
logT s = liftF $ Log s unit

feedbackT :: forall a. Array a -> TestM (Maybe a)
feedbackT a = liftF $ Feedback $ lan a

test :: TestM Unit
test = do
  as <- feedbackT [ 1, 2, 3 ]
  logT $ show as

interpreter :: TestF ~> Effect
interpreter = case _ of
  Log s next -> log s *> pure next -- log
  Feedback t -> pure $ unLan head t -- return head

main :: Effect Unit
main = do
  let
    logT = 2
  -- Result:  Just 1
  foldFree interpreter test
