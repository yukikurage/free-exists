module Main where

import Prelude

import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.ST (ST, run)
import Data.Array (concat, head, (..))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IntPlus as IntPlus
import Lan (Lan, lan, unLan)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, modify)
import Type.Proxy (Proxy(..))

data TestF f
  = Log String f
  | Feedback (Lan Maybe Array f)
  | Traverse (Array f)

derive instance Functor TestF

type TestM = Free TestF

logT :: String -> TestM Unit
logT s = liftF $ Log s unit

headT :: forall a. Array a -> TestM (Maybe a)
headT a = liftF $ Feedback $ lan a

traverseT :: forall a. Array a -> TestM a
traverseT a = liftF $ Traverse a

test :: TestM Int
test = do
  i <- traverseT [ 1, 2, 3 ]
  j <- traverseT [ 1, 2, 3 ]
  logT $ show $ i + j
  pure $ i + j

runTestF :: forall a. TestM a -> Effect (Array a)
runTestF = resume >>> case _ of
  Right a -> pure [ a ]
  Left f -> case f of
    Log s next -> log s *> runTestF next -- log
    Feedback t -> unLan (\arr callback -> runTestF $ callback $ head arr) t -- return head
    Traverse arr -> concat <$> traverse runTestF arr -- traverse

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

y :: forall b a. b -> Maybe a
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

class Monad m <= MyMonad m where
  logSomething :: String -> m Unit
  readSomething :: m String

program :: forall m. MyMonad m => m String
program = do
  r <- readSomething
  logSomething $ r <> "!"
  pure $ "return: " <> r

--

newtype MyMonadType a = MyMonadType (ReaderT String Effect a)

derive newtype instance Functor MyMonadType
derive newtype instance Apply MyMonadType
derive newtype instance Applicative MyMonadType
derive newtype instance Bind MyMonadType
derive newtype instance Monad MyMonadType
instance MyMonad MyMonadType where
  logSomething s = MyMonadType $ liftEffect $ log s
  readSomething = MyMonadType $ ask

runMyMonadType :: forall a. String -> MyMonadType a -> Effect a
runMyMonadType s (MyMonadType m) = runReaderT m s

main = do
  res <- runMyMonadType "hello" program
  log $ res
  pure unit
