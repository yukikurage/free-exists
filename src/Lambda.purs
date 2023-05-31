{-
Lambda Calculus in PureScript
de Bruijn Index (0-indexed)
-}

module Lambda
  ( Fix(..)
  , Lambda
  , LambdaF(..)
  , addL
  , app
  , betaReduce
  , idL
  , lam
  , leftBetaReduce
  , main
  , mulL
  , mutFreeVars
  , nL
  , showLambda
  , succL
  , var
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail)
import Control.Safely (for_)
import Data.Array (elem, filter)
import Data.Identity (Identity)
import Data.List.Lazy (List, nil)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (log)

newtype Fix f = Fix (f (Fix f))

derive instance Newtype (Fix f) _

data LambdaF a
  = Var Int
  | App a a
  | Lam a

derive instance Functor LambdaF

type Lambda = Fix LambdaF

var :: Int -> Lambda
var = Fix <<< Var

app :: Lambda -> Lambda -> Lambda
app x y = Fix $ App x y

lam :: Lambda -> Lambda
lam x = Fix $ Lam x

showLambda :: Lambda -> String
showLambda lambda = case unwrap lambda of
  Var x -> show x
  App a b -> "(" <> showLambda a <> " " <> showLambda b <> ")"
  Lam a -> "(λ " <> showLambda a <> ")"

-- | 自由変数全てに適用する（beta 簡約で使う）
-- | n : 渡す λ式が n 階層目
-- | f : 適用する関数
-- | lambda : ラムダ式
mutFreeVars :: Int -> (Int -> Int) -> Lambda -> Lambda
mutFreeVars n f lambda = case unwrap lambda of
  Var x | x > n -> var $ f x
  App a b -> app (mutFreeVars n f a) (mutFreeVars n f b)
  Lam a -> lam $ mutFreeVars (n + 1) f a
  _ -> lambda

-- | Left beta reduction
-- | λ (λ (λ 0 1)) (λ 0 1) -> λ (λ (λ 0 (λ 0 3)))
-- | 右辺において自由変数の index が内側に入るたびに + 1 されることに注意（↑の場合 λ 0 1 が λ 0 3 になる）
leftBetaReduce :: Lambda -> Maybe Lambda
leftBetaReduce lambda = case unwrap lambda of
  App (Fix (Lam left)) right -> Just $ go 0 $ mutFreeVars 1 (_ - 1) left
    where
    go :: Int -> Lambda -> Lambda
    go n l = case unwrap l of
      Var x | x == n -> mutFreeVars 0 (_ + n) right
      App a b -> app (go n a) (go n b)
      Lam a -> lam $ go (n + 1) a
      _ -> l
  App left right -> case leftBetaReduce left of
    Just l -> Just $ app l right
    Nothing -> case leftBetaReduce right of
      Just r -> Just $ app left r
      Nothing -> Nothing
  Lam a -> lam <$> leftBetaReduce a
  _ -> Nothing

betaReduce :: Lambda -> List Lambda
betaReduce lambda = case leftBetaReduce lambda of
  Just l -> l `List.cons` betaReduce l
  Nothing -> List.nil

idL :: Lambda
idL = lam $ var 0

nL :: Int -> Lambda
nL = lam <<< lam <<< go
  where
  go :: Int -> Lambda
  go 0 = var 0
  go n = app (var 1) (go (n - 1))

succL :: Lambda
succL = lam $ lam $ lam $ var 1 `app` (var 2 `app` var 1 `app` var 0)

addL :: Lambda
addL = lam $ lam $ lam $ lam $ var 3 `app` var 1 `app` (var 2 `app` var 1 `app` var 0)

-- mulL :: Lambda
-- mulL = lam $ lam $ lam $ var 2 `app` (var 1 `app` var 0)
mulL :: Lambda
mulL = lam $ lam $ var 1 `app` (addL `app` var 0) `app` nL 0

testLambda :: Lambda
testLambda = lam $ app (var 0) (var 1)

main :: Effect Unit
main = do
  log $ "id : " <> showLambda idL
  let
    should2 = succL `app` nL 1
  log $ "1 : " <> showLambda (nL 1)
  log $ "succ(1) : " <> showLambda should2
  log "beta reduce :"
  for_ (betaReduce should2) \l -> do
    log $ "  " <> showLambda l
  log $ "2 : " <> showLambda (nL 2)
  log $ "3 : " <> showLambda (nL 3)
  log $ "2 + 3 : " <> showLambda (addL `app` nL 2 `app` nL 3)
  log "beta reduce :"
  for_ (betaReduce $ addL `app` nL 2 `app` nL 3) \l -> do
    log $ "  " <> showLambda l
  log $ "5 : " <> showLambda (nL 5)
  log $ "2 * 3 : " <> showLambda (mulL `app` nL 2 `app` nL 3)
  log "beta reduce :"
  for_ (betaReduce $ mulL `app` nL 2 `app` nL 3) \l -> do
    log $ "  " <> showLambda l
  log $ "6 : " <> showLambda (nL 6)
