{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, FlexibleInstances, 
             ScopedTypeVariables, CPP, KindSignatures #-}
-- | Reflection utilities for vinyl records.
module Data.Vinyl.Reflect where
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Sum(..))
import Data.Vinyl (FieldRec)
import Foreign.Storable (Storable(sizeOf))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Data.Proxy
#endif
import GHC.TypeLits

-- | List all field names in a record.
class HasFieldNames a where
  fieldNames :: a -> [String]

instance HasFieldNames (FieldRec '[]) where
  fieldNames _ = []

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
instance (KnownSymbol sy, HasFieldNames (FieldRec ts))
  => HasFieldNames (FieldRec ('((sy::Symbol), t) ': ts)) where
  fieldNames _ = symbolVal (Proxy::Proxy sy)
                 : fieldNames (undefined::FieldRec ts)
#else
instance (SingI sy, HasFieldNames (PlainFieldRec ts))
  => HasFieldNames (FieldRec ('((sy::Symbol),t) ': ts)) where
  fieldNames _ = fromSing (sing::Sing sy)
                 : fieldNames (undefined::PlainFieldRec ts)
#endif

-- | Compute the size in bytes of of each field in a record.
class HasFieldSizes a where
  fieldSizes :: a -> [Int]

instance HasFieldSizes (FieldRec '[]) where
  fieldSizes _ = []

instance (HasFieldSizes (FieldRec ts), Storable t)
  => HasFieldSizes (FieldRec ('((sy::Symbol), t) ': ts)) where
  fieldSizes _ = sizeOf (undefined::t)
                 : fieldSizes (undefined::FieldRec ts)

-- | Compute the dimensionality of each field in a record. This is
-- primarily useful for things like the small finite vector types
-- provided by "Linear".
class HasFieldDims a where
  fieldDims :: a -> [Int]

instance HasFieldDims (FieldRec '[]) where
  fieldDims _ = []

instance (HasFieldDims (FieldRec ts), Foldable v, Num (v a))
  => HasFieldDims (FieldRec ('((sy::Symbol), v a) ': ts)) where
  fieldDims _ = getSum (foldMap (const (Sum 1)) (0::v a))
                : fieldDims (undefined::FieldRec ts)
