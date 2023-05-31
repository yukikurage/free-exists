module CofreeLens where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, head, tail, (:<))
import Control.Comonad.Env (Env, env, runEnv)
import Control.Monad.Free (Free, liftF, resume, wrap)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Lens (Lens', _2, lens, set, view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- f と g が互いに Getter Setter になっているとき、その関係を Free と Cofree に持ち上げる
liftLensF' :: forall f g. Functor f => Functor g => (forall x y. Lens' (f x /\ g y) (x /\ y)) -> forall x y. Lens' (Free f x /\ Cofree g y) (x /\ y)
liftLensF' lns = lens getter setter
  where
  getter :: forall x y. Free f x /\ Cofree g y -> x /\ y
  getter (fr /\ cf) = case resume fr of
    Left next -> getter $ view lns $ next /\ tail cf
    Right x -> x /\ head cf

  setter :: forall x y. Free f x /\ Cofree g y -> x /\ y -> Free f x /\ Cofree g y
  setter (fr /\ cf) (x /\ y) = case resume fr of
    Left next -> wrap frSet /\ (head cf :< cfSet)
      where
      viewed = view lns $ next /\ tail cf
      frSet /\ cfSet = set lns (setter viewed $ x /\ y) $ next /\ tail cf
    Right _ -> pure x /\ (y :< tail cf)

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

-- Env と Reader のお互いへの Lens を作る
readerEnvLens :: forall x a b. Eq x => Lens' (Env x a /\ Reader x b) (a /\ b)
readerEnvLens = lens getter setter
  where
  getter :: Env x a /\ Reader x b -> a /\ b
  getter (ev /\ rd) = a /\ runReader rd x
    where
    x /\ a = runEnv ev

  setter :: Env x a /\ Reader x b -> a /\ b -> Env x a /\ Reader x b
  setter (ev /\ rd) (newA /\ newB) = env x newA /\ (asks \x' -> if x' == x then newB else runReader rd x')
    where
    x /\ _ = runEnv ev

-- 上で作った Lens を使って CoMoore と Moore のお互いへの Lens を作る
mooreCoMooreLens :: forall x a b. Eq x => Lens' (CoMoore x a /\ Moore x b) (a /\ b)
mooreCoMooreLens = liftLensF' readerEnvLens

-- CoMoore の構成
transition :: forall x. x -> Free (Env x) Unit
transition x = liftF $ env x unit

-- 例として Moore マシンを作成する
-- イベントとして Int が与えられるので、状態に足していく、 View は状態をそのまま返す
mooreMachine :: Moore Int Int
mooreMachine = buildMoore update view 0
  where
  update :: Int -> Int -> Int
  update x s = s + x

  view :: Int -> Int
  view s = s

-- 例 CoMoore
-- モナドで構成できる
coMooreMachine :: CoMoore Int Unit
coMooreMachine = do
  transition 2
  transition 3
  transition 1

-- 確認
main :: Effect Unit
main = do
  logShow $ viewMoore [ 2, 3, 1 ] mooreMachine -- 6
  logShow $ viewMoore [ 1, 2, 4 ] mooreMachine -- 7
  logShow $ view (mooreCoMooreLens <<< _2) $ coMooreMachine /\ mooreMachine -- 6

  -- Moore マシンの一部だけ更新する
  let
    _ /\ moore2 = set (mooreCoMooreLens <<< _2) 10000 $ coMooreMachine /\ mooreMachine

  logShow $ viewMoore [ 2, 3, 1 ] moore2 -- 10000
  logShow $ viewMoore [ 1, 2, 4 ] moore2 -- 7 のまま
