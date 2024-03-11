module AutoDiff.Test where

import Prelude

import AutoDiff.Evaluate (evaluate)
import AutoDiff.Smooth (Smooth, c, n, (:*), (:+))
import Run (Run, extract)

-- | 1 + x^3 + -y^2
-- | where x = 2, y = 4
testF :: forall r x. Run (smooth :: Smooth x | r) x
testF = c 1 :+ (c 2 :* c 2 :* c 2) :+ n (c 4 :* c 4)

evalTestF :: Int
evalTestF = extract $ evaluate testF
