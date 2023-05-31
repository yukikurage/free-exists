## はじめに

どうも 21B の yukikurage です。特にココに書くこともないので本題を始めます。

## Functor の Paring

[purescript-pairing](https://pursuit.purescript.org/packages/purescript-pairing) では Functor `f` `g` が Pairing していることを

```haskell
type Pairing f g = forall c b a. (a -> b -> c) -> f a -> g b -> c
```

型の関数が存在する事で定義しています。さらに `Pairing`　を演算子化しています

```haskell
f ⋈ g = Pairing f g
```

このままでは Pairing がどの様なものか分かりにくいですが、次の関数を見ると分かりやすいかなと思います

```haskell
zap :: forall f g a b. f ⋈ g -> f (a -> b) -> g a -> b
zap pairing = pairing ($)
```

`a` が `g` でラップされているので本来 `a -> b` にそのまま適用できませんが、`f` によって `g` を "解釈" して、`a`　型の値を取り出し、それを `a -> b` に適用して `b` にしていると見れます。

これはプログラム `g a` をインタプリタ `f (a -> b)` で解釈して、`b` を得る、と見ることができます。（比喩ですが）

特に `g` がモナド、`f` がコモナドの場合はこのアナロジーが使いやすいです。（モナドは一種の DSL で書かれたプログラムとしてみれるので）

具体的な値を入れて確認してみましょう。

## `Tuple s ⋈　Function s`

```
tupleFunc :: forall s. Tuple s ⋈ Function s
tupleFunc f (Tuple s a) g = f a (g s)
```

`Function s` は `s -> _` ですが、これはモナドになります。いわゆる `Reader` モナドですね。`s` 型の大域変数を読み込める DSL です。`Tuple s` は `s` 型の大域変数を提供します。これはいわゆる `Env` コモナドになることが知られています。

簡単なプログラムを書いてみます。

```haskell
program :: Int -> Int
program = do
  x <- (_ + 1)
  y <- (_ * 2)
  pure $ x + y
```

大域変数を `s` として `(s + 1) + (s * 2)` を計算するプログラムです。

インタプリタも書いてみましょう。これを実行するには大域変数 `s` を提供する必要があります。

```haskell
interpreter :: Tuple Int (Int -> String)
interpreter = Tuple 10 \x -> "result: " <> show x
```

大域変数として `10` を提供しました。また、プログラムの実行結果を `x` として `result: x` という文字列に変換します。

あとは `zap` で実行をするだけです。

```haskell
result :: String
result = zap tupleFunc interpreter program
```

これで `resultT == "result: 31"` となります。

このようにモナドでプログラムを記述し、それに Pairing するコモナドで解釈するという比喩が使えます。コモナドとモナドの Pairing は多くあることが知られているので、見ていきましょう。

## `Identity ⋈　Identity`

もっとも単純な Paring です。

```haskell
identity :: Identity ⋈ Identity
identity f (Identity a) (Identity b) = f a b
```

プログラムを書いてみます

```haskell
program :: Identity Int
program = do
  x <- Identity $ 10
  y <- Identity $ 20
  pure $ x + y
```

`Identity` はモナドにこそなりますが、特に機能はありません。

```haskell
interpreter :: Identity (Int -> String)
interpreter = Identity \x -> "result: " <> show x
```

インタプリタも特に何かする事はありません。出力をそのまま文字列に変換するだけです。

```haskell
result :: String
result = zap identity interpreter program
```

これで `result == "result: 30"` となります。

## `Function s ⋈ Tuple s` (`Monoid s`)

これは `Tuple s ⋈ Function s` の逆です。`Tuple s` は `s` が `Monoid` の時にいわゆる `Write` モナドになります。`Function s` は `s` が `Monoid` の時にいわゆる `Traced` コモナドになります。

```haskell
funcTuple  :: forall s. Function s ⋈ Tuple s
funcTuple f g (Tuple s b) = f (g s) b
```

プログラムを書いてみましょう

```haskell
program :: Tuple String Int
program = do
  x <- Tuple "x" $ 10
  y <- Tuple "y" $ 20
  pure $ x + y
```

`"x"` と `"y"` を書き込みつつ `10 + 20` を計算する `Write` モナドです

```haskell
interpreter :: String -> Int -> String
interpreter s x = "log: " <> s <> ", result: " <> show x
```

インタプリタはログ `s` と実行結果 `x` を受け取り、`"log: " <> s <> ", result: " <> show x` という文字列を返します。

```haskell
result :: String
result = zap funcTuple interpreter program
```

これで `result == "log: xy, result: 30"` となります。

## `f ⋈ g -> Free f ⋈ Cofree g`

`f` と `g` がペアリングしているとき、モナド `Free` とコモナド `Cofree` によってそれらを拡張することができます。

```haskell

```
