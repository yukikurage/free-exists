module Loop where

import Prelude

import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)

data Command a f
  = Label f
  | Goto f
  | Write a f
  | Read (a -> f)

derive instance Functor (Command a)

type Program a = Free (Command a)

label :: forall a. Program a Unit
label = liftF $ Label unit

goto :: forall a. Program a Unit
goto = liftF $ Goto unit

write :: forall a. a -> Program a Unit
write a = liftF (Write a unit)

read :: forall a. Program a a
read = liftF (Read identity)

interpreter :: forall a. a -> Program a Unit -> a
interpreter initState initProgram =
  let
    go :: a -> Program a Unit -> Program a Unit -> a
    go state lbl prg = case resume prg of
      Right _ -> state
      Left (Label f) -> go state prg f
      Left (Goto _) -> go state lbl lbl
      Left (Write a f) -> go a lbl f
      Left (Read f) -> go state lbl (f state)
  in
    go initState (pure unit) initProgram

test :: Program Int Unit
test = do
  label
  x <- read
  when (x < 10) do
    write (x + 3)
    goto

main :: Effect Unit
main = do
  let
    result = interpreter 0 test
  log $ "result: " <> show result -- result: 12