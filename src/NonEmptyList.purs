module NonEmptyList where

import Prelude

import Stream (Shift)

data ShiftWithFallback a = Fallback a | WithShift (Shift a)
