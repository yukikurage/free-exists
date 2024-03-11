module GratePlayGround where

import Prelude

import Data.Either (Either)
import Data.Foldable (intercalate, sum)
import Data.Generic.Rep (class Generic)
import Data.Lens (Grate, Lens, _Left, over, review, zipFWithOf)
import Data.Lens.Grate (cotraversed, grate)
import Data.Lens.Record (prop)
import Data.Monoid (power)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))

-- | Grate Optics を使う
-- | 関数を扱うのが一般的？

data Pair a = Pair a a

derive instance Generic (Pair a) _
instance Show a => Show (Pair a) where
  show = genericShow

pFst :: forall a. Pair a -> a
pFst (Pair x _) = x

pSnd :: forall a. Pair a -> a
pSnd (Pair _ y) = y

-- | Grate Optics の定義
pairGrate :: forall a b. Grate (Pair a) (Pair b) a b
pairGrate = grate \f -> Pair (f pFst) (f pSnd)

-- | Review の操作
-- | == Pair 1 1
pairReviewTest :: Pair Int
pairReviewTest = review pairGrate 1

-- | Setter の操作
-- | == Pair 2 3
pairSetterTest :: Pair Int
pairSetterTest = over pairGrate (_ + 1) (Pair 1 2)

-- | Zipper の操作
-- | == Pair 4 5
pairZipperTest :: Pair Int
pairZipperTest = zipFWithOf pairGrate sum [ Pair 1 3, Pair 3 2 ]

-- | 関数でも使える
type NiceFunction a = Int -> a

-- | 関数ばっかでややこしくなる
-- | f :: (NiceFunction a -> a) -> a
-- | f から NiceFunction a を作る
niceFunctionGrate :: forall a b. Grate (NiceFunction a) (NiceFunction b) a b
niceFunctionGrate = grate \f -> \n -> f \nf -> nf n

-- | 次のようにも定義できて便利！
niceFunctionGrate2 :: forall a b. Grate (NiceFunction a) (NiceFunction b) a b
niceFunctionGrate2 = cotraversed

-- | niceFunctionHello 1 == "Hello"
-- | niceFunctionHello 4 == "Helloooo"
niceFunctionHello :: NiceFunction String
niceFunctionHello n = "Hell" <> power "o" n

-- | niceFunctionHello 1 == "World"
-- | niceFunctionHello 4 == "Worrrrld"
niceFunctionWorld :: NiceFunction String
niceFunctionWorld = \n -> "Wo" <> power "r" n <> "ld"

-- | Review の操作
-- | 常に "Const" を返す関数になる
niceFunctionReviewTest :: NiceFunction String
niceFunctionReviewTest = review niceFunctionGrate "Const"

-- | Setter の操作
-- | 結果の値に ! を追加
-- | niceFunctionSetterTest 1 == "Hello!"
-- | niceFunctionSetterTest 4 == "Helloooo!"
niceFunctionSetterTest :: NiceFunction String
niceFunctionSetterTest = over niceFunctionGrate (_ <> "!") niceFunctionHello

-- | Zipper の操作
-- | niceFunctionZipperTest 1 == "Hello World"
-- | niceFunctionZipperTest 4 == "Helloooo Worrrrld"
niceFunctionZipperTest :: NiceFunction String
niceFunctionZipperTest = zipFWithOf niceFunctionGrate (intercalate " ") [ niceFunctionHello, niceFunctionWorld ]

type WithLens a = NiceFunction { hello :: a, world :: String }

helloLens :: forall a b r. Lens { hello :: a | r } { hello :: b | r } a b
helloLens = prop (Proxy :: Proxy "hello")

-- | hello にそのまま n を突っ込む NiceFunction
before :: WithLens Int
before = \n -> { hello: n, world: "world" }

-- | 結果の hello に show を適用
after :: WithLens String
after = over (niceFunctionGrate <<< helloLens) show before

type WithPrism a = NiceFunction (Either a Int)

-- | 常に `Left "I think I'm on the left."` を返す
reviewed :: WithPrism String
reviewed = review (niceFunctionGrate <<< _Left) "I think I'm on the left."

type NiceFunctionPair a = Pair (NiceFunction a)

-- | Review の操作
-- | 常に "Const" を返す関数の Pair ができる
niceFunctionPairReviewTest :: NiceFunctionPair String
niceFunctionPairReviewTest = review (pairGrate <<< niceFunctionGrate) "Const"

niceFunctionPair :: NiceFunctionPair String
niceFunctionPair = Pair niceFunctionHello niceFunctionWorld

-- | Setter の操作
-- | Pair のそれぞれの NiceFunction の結果に ! を追加
niceFunctionPairSetterTest :: NiceFunctionPair String
niceFunctionPairSetterTest = over (pairGrate <<< niceFunctionGrate) (_ <> "!") niceFunctionPair

-- | Zipper の操作
-- | 結果の値 Pair f g に対して
-- | f 5 == "Hellooooo Hellooooo"
-- | g 5 == "Worrrrrld Worrrrrld"
niceFunctionPairZipperTest :: NiceFunctionPair String
niceFunctionPairZipperTest = zipFWithOf (pairGrate <<< niceFunctionGrate) (intercalate " ") niceFunctionPair
