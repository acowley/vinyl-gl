{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, FlexibleInstances, 
             ScopedTypeVariables, CPP #-}
-- | Reflection utilities for vinyl records.
module Data.Vinyl.Reflect where
import Data.Foldable (Foldable, foldMap)
import Data.Functor.Identity
import Data.Monoid (Sum(..))
import Data.Vinyl (Rec, PlainRec, (:::))
import Foreign.Storable (Storable(sizeOf))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Data.Proxy
#endif
import GHC.TypeLits

-- | List all field names in a record.
class HasFieldNames a where
  fieldNames :: a -> [String]

instance HasFieldNames (Rec '[] f) where
  fieldNames _ = []

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
instance (KnownSymbol sy, HasFieldNames (Rec ts Identity))
  => HasFieldNames (Rec ((sy:::t) ': ts) Identity) where
  fieldNames _ = symbolVal (Proxy::Proxy sy) : fieldNames (undefined::PlainRec ts)
#else
instance (SingI sy, HasFieldNames (Rec ts Identity))
  => HasFieldNames (Rec ((sy:::t) ': ts) Identity) where
  fieldNames _ = fromSing (sing::Sing sy) : fieldNames (undefined::PlainRec ts)
#endif

-- | Compute the size in bytes of of each field in a record.
class HasFieldSizes a where
  fieldSizes :: a -> [Int]

instance HasFieldSizes (Rec '[] f) where
  fieldSizes _ = []

instance (HasFieldSizes (Rec ts Identity), Storable t)
  => HasFieldSizes (Rec ((sy:::t) ': ts) Identity) where
  fieldSizes _ = sizeOf (undefined::t) : fieldSizes (undefined::PlainRec ts)

-- | Compute the dimensionality of each field in a record. This is
-- primarily useful for things like the small finite vector types
-- provided by "Linear".
class HasFieldDims a where
  fieldDims :: a -> [Int]

instance HasFieldDims (Rec '[] f) where
  fieldDims _ = []

instance (HasFieldDims (PlainRec ts), Foldable v, Num (v a))
  => HasFieldDims (PlainRec (sy:::v a ': ts)) where
  fieldDims _ = getSum (foldMap (const (Sum 1)) (0::v a))
                : fieldDims (undefined::PlainRec ts)
