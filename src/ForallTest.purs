module ForallTest where

import Prelude

import Data.Identity (Identity(..))

class Monad m <= MonadHoge m where
  hogeM :: m Int

newtype M1 a = M1 (Identity a)

derive newtype instance Functor M1
derive newtype instance Apply M1
derive newtype instance Applicative M1
derive newtype instance Bind M1
derive newtype instance Monad M1

instance MonadHoge M1 where
  hogeM = pure 1

runM1 :: forall a. M1 a -> a
runM1 (M1 (Identity a)) = a

newtype M2 a = M2 (Identity a)

derive newtype instance Functor M2
derive newtype instance Apply M2
derive newtype instance Applicative M2
derive newtype instance Bind M2
derive newtype instance Monad M2

instance MonadHoge M2 where
  hogeM = pure 2

runM2 :: forall a. M2 a -> a
runM2 (M2 (Identity a)) = a

action :: forall m. Bind m => MonadHoge m => m Int
action = do
  hoge <- hogeM
  pure $ hoge * 2

testF :: (forall m. MonadHoge m => m Int) -> Int
testF act = runM1 act + runM2 act
