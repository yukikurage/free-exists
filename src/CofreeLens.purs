module CofreeLens where

import Prelude

import Control.Comonad (class Comonad, duplicate, extract)
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail, (:<))
import Control.Comonad.Env (Env, env, runEnv)
import Control.Monad.Free (Free, liftF, resume, wrap)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.Identity (Identity(..))
import Data.Lens (Lens', _2, lens, preview, set, view)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.Types (AffineTraversal')
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- Lens' を Free Cofree に昇格する
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

-- AffineTraversal' の関係を Free Cofree に昇格する
liftAffineTraversalF' :: forall f g. Functor f => Functor g => (forall x y. AffineTraversal' (f x /\ g y) (x /\ y)) -> forall x y. AffineTraversal' (Free f x /\ Cofree g y) (x /\ y)
liftAffineTraversalF' aft = affineTraversal setter getter
  where
  getter :: forall x y. Free f x /\ Cofree g y -> Either (Free f x /\ Cofree g y) (x /\ y)
  getter x = note x $ getter' x

  getter' :: forall x y. Free f x /\ Cofree g y -> Maybe (x /\ y)
  getter' (fr /\ cf) = case resume fr of
    Left next -> getter' =<< preview aft (next /\ tail cf)
    Right x -> Just $ x /\ head cf

  setter :: forall x y. Free f x /\ Cofree g y -> x /\ y -> Free f x /\ Cofree g y
  setter (fr /\ cf) (x /\ y) = case resume fr of
    Left next -> case preview aft $ next /\ tail cf of
      Just viewed -> wrap frSet /\ (head cf :< cfSet)
        where
        frSet /\ cfSet = set aft (setter viewed $ x /\ y) $ next /\ tail cf
      Nothing -> (fr /\ cf)
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

-- Cofree って Cofree Comonad って事？？

leftAd :: forall f m. Functor f => Comonad m => (m ~> f) -> (m ~> Cofree f)
leftAd f cm = buildCofree (\m -> Tuple (extract m) $ f (duplicate m)) cm

rightAd :: forall f m. Functor f => Comonad m => (m ~> Cofree f) -> (m ~> f)
rightAd f cm = map head $ tail $ f cm

cofreeUnit :: forall m. Comonad m => m ~> Cofree m
cofreeUnit cm = buildCofree (\m -> Tuple (extract m) $ duplicate m) cm

cofreeCounit :: forall f. Functor f => Cofree f ~> f
cofreeCounit cf = map head $ tail cf

{-
rightAd cofreeUnit cm
  = map head $ tail $ cofreeUnit cm
  = map head $ tail $ buildCofree (\m -> Tuple (extract m) $ duplicate m) cm
  = map head $ map (\cm' -> buildCofree (\m -> Tuple (extract m) $ duplicate m) cm') $ duplicate cm
  = map (head . (\cm' -> buildCofree (\m -> Tuple (extract m) $ duplicate m) cm')) $ duplicate cm
  = map extract $ duplicate cm
  = cm
-}

{-
leftAd cofreeCounit cf
  = buildCofree (\m -> Tuple (extract m) $ cofreeCounit $ duplicate m) cf
  = buildCofree (\m -> Tuple (extract m) $ map head $ tail $ duplicate m) cf
  = buildCofree (\m -> Tuple (head m) $ tail m) cf
  = cf
-}

-- leftAd の使い道
-- 案の定 Moore マシンかも

-- まず 定義に使う Comonad を選ぶ
-- ココでは Stream とする
type Stream a = Cofree Identity a

-- Stream の構成
buildStream :: forall s a. s -> (s -> a /\ s) -> Stream a
buildStream init next = buildCofree (\s -> Tuple (fst $ next s) $ Identity $ snd $ next s) init

data Nat = Z | S Nat

-- Stream から値を取り出す
ixStream :: forall a. Nat -> Stream a -> a
ixStream Z stream = head stream
ixStream (S n) stream = ixStream n $ unwrap $ tail stream

strLen :: String -> Nat
strLen str = case String.uncons str of
  Nothing -> Z
  Just { tail: str' } -> S $ strLen str'

-- Stream a から String -> a への変換とかをする
-- この場合、String の長さを取得して次に移る
ixStreamFromStrLen :: forall a. Stream a -> Reader String a
ixStreamFromStrLen stream = asks \str -> ixStream (strLen str) stream

-- 試しに Stream を作ってみる
-- ここでは 2 倍にしていく
stream :: Stream Int
stream = buildStream 1 (\s -> s /\ 2 * s)

-- leftAd があれば ixStreamFromStrLen を Stream a -> Moore String a に変換可能！
someMoore :: Moore String Int
someMoore = leftAd ixStreamFromStrLen stream

-- Free の fold

type ComposeF :: forall k1 k2 k3. (k1 -> k2) -> (k3 -> k1) -> k3 -> k2
type ComposeF f g a = f (g a)

infixr 5 type ComposeF as :<:

foldrF :: forall f g. Functor f => Functor g => (f :<: g ~> g) -> (Identity ~> g) -> Free f ~> g
foldrF joinF pureF free = case resume free of
  Right x -> pureF $ Identity x
  Left next -> joinF $ map (foldrF joinF pureF) next

foldr :: forall b a. (a /\ b -> b) -> (Unit -> b) -> Array a -> b
foldr f g xs = case uncons xs of
  Nothing -> g unit
  Just { head: h, tail: t } -> f $ map (foldr f g) tp
    where
    tp = h /\ t
