module CofreeLens where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, explore, head, tail, (:<))
import Control.Comonad.Env (Env, env)
import Control.Monad.Free (Free, liftF, resume, resume')
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Functor.Pairing (Pairing, zap)
import Data.Functor.Pairing as Pairing
import Data.Lens (Lens, Lens', lens, set, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- f の Lens' の Free を Cofree の Lens' に変換する
freeToLens'
  :: forall f g a
   . Functor f
  => Functor g
  => (forall x y z. (x -> y -> z) -> f x -> g y -> z) -- Getter 用の Pair
  -> (forall x y z. (x -> z) -> (y -> z) -> f x -> g y -> Tuple (f z) (g z)) -- Setter 用の Pair
  -> Tuple (f z) (g z)
  -> Free g Unit
  -> Lens' (Cofree f a) a
freeToLens' getPair setPair free = lens (get free) set
  where
  get :: Free g Unit -> Cofree g a -> a
  get fr cf = case resume fr of
    Left (next :: g (Free g Unit)) -> get nx tl
      where
      Tuple tl nx = lns next $ tail cf
    Right _ -> head cf

  set :: Free g Unit -> a -> Cofree g a -> Cofree g a
  set fr a cf = case resume fr of
    Left (next :: g (Free g Unit)) -> head cf :< set
      where
      Tuple

{-
実際に使ってみる
-}

-- Moore マシンを作る
type Moore x a = Cofree (Reader x) a

-- Moore マシンの構成
buildMoore :: forall s x a. (x -> s -> s) -> (s -> a) -> s -> Moore x a
buildMoore update view init = buildCofree (\state -> Tuple (view state) $ asks \x -> update x state) init

-- Moore マシンから直接値を取り出す
viewMoore :: forall x a. Array x -> Moore x a -> a
viewMoore xs moore = case uncons xs of
  Nothing -> head moore
  Just { head: x, tail: xs' } -> viewMoore xs' $ runReader (tail moore) x

-- CoMoore も作っておく
type CoMoore x a = Free (Env x) a

-- Pairing (Env x) (Reader x) を作っておく（あとで使う）
envReader :: forall x a b c. (a -> b -> c) -> Env x a -> Reader x b -> c
envReader = Pairing.sym (Pairing.readerEnv Pairing.identity)

-- CoMoore の構成
transition :: forall x. x -> Free (Env x) Unit
transition x = liftF $ env x unit

-- 複数回 transition する
transitions :: forall x. Array x -> Free (Env x) Unit
transitions xs = traverse_ transition xs

-- 例として Moore マシンを作成する
-- イベントとして Int が与えられるので、状態に足していく、 View は状態をそのまま返す
mooreMachine :: Moore Int Int
mooreMachine = buildMoore update view 0
  where
  update :: Int -> Int -> Int
  update x s = s + x

  view :: Int -> Int
  view s = s

-- Lens' を作成する
-- ここでは例として 2, 3, 1 と遷移する
mooreLens :: Lens' (Moore Int Int) Int
mooreLens = freeToLens' envReader $ transitions [ 2, 3, 1 ]

-- mooreLens で一部だけ値を替える
mooreMachine2 :: Moore Int Int
mooreMachine2 = {- set mooreLens 10000 mooreMachine -}  embed (zap envReader) (transitions [ 2, 3, 1 ] $> 10000) mooreMachine

-- 確認
main :: Effect Unit
main = do
  logShow $ viewMoore [ 2, 3, 1 ] mooreMachine -- 6
  logShow $ viewMoore [ 1, 2, 4 ] mooreMachine -- 7
  logShow $ view mooreLens mooreMachine -- 6

  logShow $ viewMoore [ 2, 3, 1 ] mooreMachine2 -- 10000
  logShow $ viewMoore [ 1, 2, 4 ] mooreMachine2 -- 7 のまま
