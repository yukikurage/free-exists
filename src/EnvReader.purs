module EnvReader where

import Prelude

import Control.Comonad (class Comonad, duplicate, extract)
import Control.Extend (class Extend)
import Data.Functor.Pairing.Co (Co, co, runCo)
import Data.Tuple (Tuple(..))

-- Co Reader = Env か確かめる

newtype Reader r a = Reader (r -> a)

derive newtype instance Functor (Reader r)
derive newtype instance Apply (Reader r)
derive newtype instance Applicative (Reader r)
derive newtype instance Bind (Reader r)
derive newtype instance Monad (Reader r)

newtype Env r a = Env (Tuple r a)

derive newtype instance Functor (Env r)
derive newtype instance Extend (Env r)
derive newtype instance Comonad (Env r)

envToCoReader :: forall r. Env r ~> Co (Reader r)
envToCoReader (Env (Tuple r a)) = co \(Reader reader) -> reader r a

coReaderToEnv :: forall r. Co (Reader r) ~> Env r
coReaderToEnv coReader = Env $ runCo coReader $ Reader Tuple

extractCoReader :: forall r a. Co (Reader r) a -> a
extractCoReader coReader = extract $ coReaderToEnv coReader

{-
extract $ coReaderToEnv coReader
= extract $ Env $ runCo coReader $ Reader Tuple
= extract $ runCo coReader $ Reader Tuple
= runCo coReader $ Reader \_ -> identity
= runCo coReader $ pure identity
-}

duplicateCoReader :: forall r a. Co (Reader r) a -> Co (Reader r) (Co (Reader r) a)
duplicateCoReader coReader = map envToCoReader $ envToCoReader $ duplicate $ coReaderToEnv coReader

{-
map envToCoReader $ envToCoReader $ duplicate $ coReaderToEnv coReader
= map envToCoReader $ envToCoReader $ (\(Tuple e a) -> Tuple e (Tuple e a)) $ (f :: forall t. Reader r (a -> t) -> t) $ Reader \r -> \a -> Tuple r a
= map envToCoReader $ envToCoReader $ (f :: forall t. Reader r (a -> t) -> t) $ Reader \r -> \a -> Tuple r (Tuple r a)
= map envToCoReader $ (f :: forall t. Reader r (a -> t) -> t) $ Reader \r -> \a -> co \(Reader reader) -> reader r (Tuple r a)
= (f :: forall t. Reader r (a -> t) -> t) $ Reader \r -> \a -> co \(Reader reader) -> reader r $ co \(Reader reader') -> reader' r a
= runCo coReader $ Reader \r -> \a -> co \(Reader reader) -> reader r $ co \(Reader reader') -> reader' r a

\r -> \a -> \f -> f r \g -> g r a

はどんな構造か　これを pure、join だけで作れる？

これを

pure = \a -> \r -> a
join = \f -> \r -> f r r
map = \f -> \g -> \r -> f (g r)
apply = \f -> \g -> \r -> f r (g r)

だけで作れる？

ちなみに r が無いと

\a -> \f -> f \g -> g a

を
pure' = \a -> a
join' = \a -> a
map' = \f -> f
apply' = \f -> f

で作ることになる　　　無理では？

solve a f = f \g -> g a

\f -> f ans

solve ans = \f -> f \g -> g ans
-}
