module ExistsClass where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

{-
Haskell の GADTs

data T where
  C :: forall a. Show a => a -> T

みたいなのを再現したい

すなわち Show インスタンスは全て入るような型
-}

foreign import data Inst :: Type
foreign import data Value :: Type

newtype ExistsShow = ExistsShow { inst :: Inst, value :: Value }

mkExistsShow :: forall a. Show a => a -> ExistsShow
mkExistsShow = unsafeCoerce \inst value -> { inst, value } -- Show インスタンスを持つ

runExistsShow :: forall b. (forall a. Show a => a -> b) -> ExistsShow -> b
runExistsShow f (ExistsShow { inst, value }) = coerceF f (spy "Inst" inst) value
  where
  coerceF :: (forall a. Show a => a -> b) -> Inst -> Value -> b
  coerceF = unsafeCoerce

main :: Effect Unit
main = do
  let x = mkExistsShow 1
  log $ runExistsShow show x
