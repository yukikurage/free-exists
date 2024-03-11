module AutoDiff.ReaderTest where

import Prelude

import Data.Functor.Variant (VariantF, expand, inj)
import Data.Maybe (Maybe)
import Prim.Row (class Union)
import Prim.RowList (Cons)
import Run (Run, VariantF, interpret, on, send)
import Run.Reader (Reader(..), READER, _reader, ask)
import Type.Row (type (+))

testHandler :: forall r. Union r (READER Int + ()) (READER Int + r) => Run (READER String + r) ~> Run (READER Int + r)
testHandler = interpret (on _reader handler (send <<< expand'))
  where
  expand' :: VariantF r ~> VariantF (READER Int + r)
  expand' = expand

  handler :: Reader String ~> Run (READER Int + r)
  handler = case _ of
    Reader k -> do
      x <- ask
      pure $ k (show x)
