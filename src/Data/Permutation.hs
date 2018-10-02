-- | Safely handle permutations with permutation-safe.
-- Conversion between standard permutation forms.
-- Helpful errors are given when trying to construct permutations with invalid data.
module Data.Permutation 
  (
  -- * The Permutation Type
    Permutation()
  -- ** Error Type
  , PermutationError(..)
  -- ** Smart Constructors
  , fromCycles
  , fromRelation
  , fromTwoLineForm
  -- ** Accessors
  , toFunction 
  , toCycles
  , toCyclesCanonical
  , toRelation
  , toTwoLineForm
  , permutedPoints
  -- * Permutation Utilities
  , inverse
  , size
  , (^^^)
  ) where
import Data.Permutation.Internal

