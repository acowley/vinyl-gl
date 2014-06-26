{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, FlexibleInstances, 
             ScopedTypeVariables, CPP, KindSignatures #-}
-- | Reflection utilities for vinyl records.
module Data.Vinyl.Reflect where
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Sum(..))
import Data.Vinyl (FieldRec, PlainFieldRec)
import Data.Vinyl.Universe ((:::))
import Foreign.Storable (Storable(sizeOf))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Data.Proxy
#endif
import GHC.TypeLits

-- | List all field names in a record.
class HasFieldNames a where
  fieldNames :: a -> [String]

instance HasFieldNames (FieldRec f '[]) where
  fieldNames _ = []

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
instance (KnownSymbol sy, HasFieldNames (PlainFieldRec ts))
  => HasFieldNames (PlainFieldRec (((sy::Symbol):::t) ': ts)) where
  fieldNames _ = symbolVal (Proxy::Proxy sy)
                 : fieldNames (undefined::PlainFieldRec ts)
#else
instance (SingI sy, HasFieldNames (PlainFieldRec ts))
  => HasFieldNames (PlainFieldRec (((sy::Symbol):::t) ': ts)) where
  fieldNames _ = fromSing (sing::Sing sy)
                 : fieldNames (undefined::PlainFieldRec ts)
#endif

-- | Compute the size in bytes of of each field in a record.
class HasFieldSizes a where
  fieldSizes :: a -> [Int]

instance HasFieldSizes (FieldRec f '[]) where
  fieldSizes _ = []

instance (HasFieldSizes (PlainFieldRec ts), Storable t)
  => HasFieldSizes (PlainFieldRec (((sy::Symbol) ::: t) ': ts)) where
  fieldSizes _ = sizeOf (undefined::t)
                 : fieldSizes (undefined::PlainFieldRec ts)

-- | Compute the dimensionality of each field in a record. This is
-- primarily useful for things like the small finite vector types
-- provided by "Linear".
class HasFieldDims a where
  fieldDims :: a -> [Int]

instance HasFieldDims (FieldRec f '[]) where
  fieldDims _ = []

instance (HasFieldDims (PlainFieldRec ts), Foldable v, Num (v a))
  => HasFieldDims (PlainFieldRec ((sy::Symbol):::v a ': ts)) where
  fieldDims _ = getSum (foldMap (const (Sum 1)) (0::v a))
                : fieldDims (undefined::PlainFieldRec ts)
