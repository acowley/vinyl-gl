{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, FlexibleInstances,
             GADTs, ScopedTypeVariables, ConstraintKinds #-}
-- | Tools for binding vinyl records to GLSL program uniform
-- parameters. The most common usage is to use the 'setUniforms'
-- function to set each field of a 'PlainRec' to the GLSL uniform
-- parameter with the same name. This verifies that each field of the
-- record corresponds to a uniform parameter of the given shader
-- program, and that the types all agree.
module Graphics.VinylGL.Uniforms (setAllUniforms, setSomeUniforms, setUniforms,
                                  HasFieldGLTypes(..)) where
import Control.Applicative ((<$>))
import Data.Foldable (traverse_)
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Vinyl
import Graphics.GLUtil (HasVariableType(..), ShaderProgram(..), AsUniform(..))
import Graphics.Rendering.OpenGL as GL
import Data.Vinyl.Reflect (HasFieldNames(..))

-- | Provide the 'GL.VariableType' of each field in a 'Rec'. The list
-- of types has the same order as the fields of the 'Rec'.
class HasFieldGLTypes a where
  fieldGLTypes :: a -> [GL.VariableType]

instance HasFieldGLTypes (Rec '[] f) where
  fieldGLTypes _ = []

instance (HasVariableType t, HasFieldGLTypes (PlainRec ts))
  => HasFieldGLTypes (PlainRec (sy:::t ': ts)) where
  fieldGLTypes _ = variableType (undefined::t) 
                   : fieldGLTypes (undefined::PlainRec ts)

type UniformFields a = (HasFieldNames a, HasFieldGLTypes a, SetUniformFields a)

-- | Set GLSL uniform parameters from a 'PlainRec'. A check is
-- performed to verify that /all/ uniforms used by a program are
-- represented by the record type. In other words, the record is a
-- superset of the parameters used by the program.
setAllUniforms :: forall ts. UniformFields (PlainRec ts)
            => ShaderProgram -> PlainRec ts -> IO ()
setAllUniforms s x = case checks of
                    Left msg -> error msg
                    Right _ -> setUniformFields locs x
  where fnames = fieldNames (undefined::PlainRec ts)
        checks = do namesCheck "record" (M.keys $ uniforms s) fnames
                    typesCheck True (snd <$> uniforms s) fieldTypes
        fieldTypes = M.fromList $
                     zip fnames (fieldGLTypes (undefined::PlainRec ts))
        locs = map (fmap fst . (`M.lookup` uniforms s)) fnames
{-# INLINE setAllUniforms #-}

-- | Set GLSL uniform parameters from a 'PlainRec' representing a
-- subset of all uniform parameters used by a program.
setUniforms :: forall ts. UniformFields (PlainRec ts)
            => ShaderProgram -> PlainRec ts -> IO ()
setUniforms s x = case checks of
                    Left msg -> error msg
                    Right _ -> setUniformFields locs x
  where fnames = fieldNames (undefined::PlainRec ts)
        checks = do namesCheck "GLSL program" fnames (M.keys $ uniforms s)
                    typesCheck False fieldTypes (snd <$> uniforms s)
        fieldTypes = M.fromList $
                     zip fnames (fieldGLTypes (undefined::PlainRec ts))
        locs = map (fmap fst . (`M.lookup` uniforms s)) fnames
{-# INLINE setUniforms #-}

-- | Set GLSL uniform parameters from those fields of a 'PlainRec'
-- whose names correspond to uniform parameters used by a program.
setSomeUniforms :: forall ts. UniformFields (PlainRec ts)
                => ShaderProgram -> PlainRec ts -> IO ()
setSomeUniforms s x = case typesCheck' True (snd <$> uniforms s) fieldTypes of
                        Left msg -> error msg
                        Right _ -> setUniformFields locs x
  where fnames = fieldNames (undefined::PlainRec ts)
        {-# INLINE fnames #-}
        fieldTypes = M.fromList . zip fnames $
                     fieldGLTypes (undefined::PlainRec ts)
        {-# INLINE fieldTypes #-}
        locs = map (fmap fst . (`M.lookup` uniforms s)) fnames
        {-# INLINE locs #-}
{-# INLINE setSomeUniforms #-}

-- | @namesCheck blame little big@ checks that each name in @little@ is
-- an element of @big@.
namesCheck :: String -> [String] -> [String] -> Either String ()
namesCheck blame little big = mapM_ aux little
  where big' = S.fromList big
        aux x | x `S.member` big' = Right ()
              | otherwise = Left $ "Field "++x++" not found in "++blame

-- | @typesChecks blame little big@ checks that each (name,type) pair
-- in @little@ is a member of @big@.
typesCheck :: Bool
           -> M.Map String GL.VariableType -> M.Map String GL.VariableType
           -> Either String ()
typesCheck blame little big = mapM_ aux $ M.toList little
  where aux (n,t)
          | Just True == (glTypeEquiv t <$> M.lookup n big) = return ()
          | otherwise = Left $ msg n (show t) (maybe "" show (M.lookup n big))
        msg n t t' = let (expected, actual) = if blame
                                              then (t,t')
                                              else (t',t)
                     in "Record and GLSL type disagreement on field "++n++
                        ": GLSL expected "++expected++
                        ", record provides "++actual

-- | @typesCheck' blame little big@ checks that each (name,type) pair
-- in the intersection of @little@ and @big@ is consistent.
typesCheck' :: Bool
            -> M.Map String GL.VariableType -> M.Map String GL.VariableType
            -> Either String ()
typesCheck' blame little big = mapM_ aux $ M.toList little
  where aux (n,t)
          | fromMaybe True (glTypeEquiv t <$> M.lookup n big) = return ()
          | otherwise = Left $ msg n (show t) (maybe "" show (M.lookup n big))
        msg n t t' = let (expected, actual) = if blame
                                              then (t,t')
                                              else (t',t)
                     in "Record and GLSL type disagreement on field "++n++
                        ": GLSL expected "++expected++
                        ", record provides "++actual

-- The equivalence on 'GL.VariableType's we need identifies Samplers
-- with Ints because this is how GLSL represents samplers.
glTypeEquiv' :: GL.VariableType -> GL.VariableType -> Bool
glTypeEquiv' GL.Sampler1D GL.Int' = True
glTypeEquiv' GL.Sampler2D GL.Int' = True
glTypeEquiv' GL.Sampler3D GL.Int' = True
glTypeEquiv' x y = x == y

-- We define our own equivalence relation on types because we don't
-- have unique Haskell representations for every GL type. For example,
-- the GLSL sampler types (e.g. Sampler2D) are just GLint in Haskell.
glTypeEquiv :: VariableType -> VariableType -> Bool
glTypeEquiv x y = glTypeEquiv' x y || glTypeEquiv' y x

class SetUniformFields a where
  setUniformFields :: [Maybe UniformLocation] -> a -> IO ()

instance SetUniformFields (Rec '[] f) where
  setUniformFields _ _ = return ()
  {-# INLINE setUniformFields #-}

instance (AsUniform t, SetUniformFields (PlainRec ts))
  => SetUniformFields (PlainRec ((sy:::t) ': ts)) where
  setUniformFields [] _ = error "Ran out of UniformLocations"
  setUniformFields (loc:locs) (Identity x :& xs) = 
    do traverse_ (asUniform x) loc
       setUniformFields locs xs
  {-# INLINABLE setUniformFields #-}
