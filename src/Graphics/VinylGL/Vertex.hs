{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, GADTs, BangPatterns,
             FlexibleInstances, FlexibleContexts, KindSignatures, RankNTypes,
             ConstraintKinds, CPP #-}
-- | Utilities for working with vertex buffer objects (VBOs) filled
-- with vertices represented as vinyl records.
module Graphics.VinylGL.Vertex (bufferVertices, bindVertices, reloadVertices,
                                deleteVertices, enableVertices, enableVertices',
                                enableVertexFields,
                                BufferedVertices(..), fieldToVAD) where
import Control.Applicative
import Control.Arrow (second)
import Data.Foldable (Foldable, foldMap)
import Data.List (find)
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as V
import Data.Vinyl (PlainFieldRec, Implicit(..), Elem)
import Data.Vinyl.Universe((:::))
import Foreign.Ptr (plusPtr)
import Foreign.Storable
import GHC.TypeLits
import Graphics.GLUtil hiding (Elem, throwError)
import Graphics.Rendering.OpenGL (VertexArrayDescriptor(..), bindBuffer,
                                  ($=), BufferTarget(..))
import qualified Graphics.Rendering.OpenGL as GL

import Data.Vinyl.Reflect
import Graphics.VinylGL.Uniforms

-- | Representation of a VBO whose type describes the vertices.
newtype BufferedVertices (fields::[*]) = 
  BufferedVertices { getVertexBuffer :: GL.BufferObject }

-- | Load vertex data into a GPU-accessible buffer.
bufferVertices :: (Storable (PlainFieldRec rs),
                   BufferSource (v (PlainFieldRec rs)))
               => v (PlainFieldRec rs) -> IO (BufferedVertices rs)
bufferVertices = fmap BufferedVertices . fromSource ArrayBuffer

-- | Reload 'BufferedVertices' with a 'V.Vector' of new vertex data.
reloadVertices :: Storable (PlainFieldRec rs)
               => BufferedVertices rs -> V.Vector (PlainFieldRec rs) -> IO ()
reloadVertices b v = do bindBuffer ArrayBuffer $= Just (getVertexBuffer b)
                        replaceVector ArrayBuffer v

-- | Delete the object name associated with 'BufferedVertices'.
deleteVertices :: BufferedVertices a -> IO ()
deleteVertices = GL.deleteObjectNames . (:[]) . getVertexBuffer

-- | Bind previously-buffered vertex data.
bindVertices :: BufferedVertices a -> IO ()
bindVertices = (bindBuffer ArrayBuffer $=) . Just . getVertexBuffer

-- | Line up a shader's attribute inputs with a vertex record. This
-- maps vertex fields to GLSL attributes on the basis of record field
-- names on the Haskell side, and variable names on the GLSL side.
enableVertices :: forall f rs. ViableVertex (PlainFieldRec rs)
               => ShaderProgram -> f rs -> IO (Maybe String)
enableVertices s _ = enableAttribs s (Proxy::Proxy (PlainFieldRec rs))

-- | Behaves like 'enableVertices', but raises an exception if the
-- supplied vertex record does not include a field required by the
-- shader.
enableVertices' :: forall f rs. ViableVertex (PlainFieldRec rs)
               => ShaderProgram -> f rs -> IO ()
enableVertices' s _ = enableAttribs s (Proxy::Proxy (PlainFieldRec rs)) >>=
                      maybe (return ()) error

-- | Produce a 'GL.VertexArrayDescriptor' for a particular field of a
-- vertex record.
fieldToVAD :: forall sy v a r rs field proxy.
              (r ~ ((sy :: Symbol) ::: v a), HasFieldNames (PlainFieldRec rs), 
               HasFieldSizes (PlainFieldRec rs), HasGLType a,
               Storable (PlainFieldRec rs), Num (v a),
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
               KnownSymbol sy,
#else
               SingI sy,
#endif
               Foldable v, Implicit (Elem r rs)) =>
              field r -> proxy (PlainFieldRec rs) -> GL.VertexArrayDescriptor a
fieldToVAD _ _ = GL.VertexArrayDescriptor dim
                                          (glType (undefined::a))
                                          (fromIntegral sz)
                                          (offset0 `plusPtr` offset)
  where sz = sizeOf (undefined::PlainFieldRec rs)
        dim = getSum $ foldMap (const (Sum 1)) (0::v a)
        Just offset = lookup n $
                      namesAndOffsets (undefined::PlainFieldRec rs)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
        n = symbolVal (Proxy::Proxy sy)
#else
        n = fromSing (sing::Sing sy)
#endif

-- Constraint alias capturing the requirements of a vertex type.
type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
                       HasFieldGLTypes t, Storable t)

-- | Bind some of a shader's attribute inputs to a vertex record. This
-- is useful when the inputs of a shader are split across multiple
-- arrays.
enableVertexFields :: forall p rs. (ViableVertex (PlainFieldRec rs))
                   => ShaderProgram -> p rs -> IO ()
enableVertexFields s _ = enableSomeAttribs s p >>= maybe (return ()) error
  where p = Proxy :: Proxy (PlainFieldRec rs)

-- | Do not raise an error is some of a shader's inputs are not bound
-- by a vertex record.
enableSomeAttribs :: forall v. ViableVertex v
                  => ShaderProgram -> Proxy v -> IO (Maybe String)
enableSomeAttribs s p = go $ fieldDescriptors (undefined::v)
  where go [] = return Nothing
        go (fd:fds) = 
          let n = fieldName fd
              shaderAttribs = attribs s
          in case M.lookup n shaderAttribs of
               Nothing -> return (Just $ "Unexpected attribute " ++ n)
               Just (_,t)
                 | fieldType fd == t -> do enableAttrib s n
                                           setAttrib s n GL.ToFloat $
                                             descriptorVAD p fd
                                           go fds
                 | otherwise -> return . Just $ "Type mismatch in " ++ n

enableAttribs :: forall v. ViableVertex v
              => ShaderProgram -> Proxy v -> IO (Maybe String)
enableAttribs s p = go (map (second snd) $ M.assocs (attribs s))
  where go [] = return Nothing
        go ((n,t):ns) = case find ((== n) . fieldName) fs of
                          Nothing -> return (Just $ "GLSL expecting "++n)
                          Just fd
                            | fieldType fd == t -> 
                              do enableAttrib s n
                                 setAttrib s n GL.ToFloat $
                                   descriptorVAD p fd
                                 go ns
                            | otherwise -> return . Just $
                                           "Type mismatch in "++n
        fs = fieldDescriptors (undefined::v)

namesAndOffsets :: (HasFieldNames t, HasFieldSizes t) => t -> [(String,Int)]
namesAndOffsets x = zip (fieldNames x) (scanl (+) 0 $ fieldSizes x)

data FieldDescriptor = FieldDescriptor { fieldName   :: String
                                       , fieldOffset :: Int
                                       , fieldDim    :: Int
                                       , fieldType   :: GL.VariableType }
                       deriving Show

fieldDescriptors :: ViableVertex t => t -> [FieldDescriptor]
fieldDescriptors x = getZipList $
                     FieldDescriptor <$> zl (fieldNames x)
                                     <*> zl (scanl (+) 0 $ fieldSizes x)
                                     <*> zl (fieldDims x)
                                     <*> zl (fieldGLTypes x)
  where zl = ZipList

descriptorVAD :: forall t a. Storable t
              => Proxy t -> FieldDescriptor -> VertexArrayDescriptor a
descriptorVAD _ fd = VertexArrayDescriptor (fromIntegral $ fieldDim fd)
                                           (variableDataType $ fieldType fd)
                                           (fromIntegral $
                                            sizeOf (undefined::t))
                                           (offset0 `plusPtr` fieldOffset fd)
