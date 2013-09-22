{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Geometry where
import Control.Applicative
import Data.Foldable (fold, foldMap)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Linear
import Graphics.VinylGL
import System.FilePath ((</>))

type Pos    = "vertexPos"    ::: V3 GLfloat
type Normal = "vertexNormal" ::: V3 GLfloat
type Color  = "vertexColor"  ::: V3 GLfloat

pos :: Pos
pos = Field

normal :: Normal
normal = Field

col :: Color
col = Field

-- The 2D corners of a square.
square :: [V2 GLfloat]
square = V2 <$> [-1,1] <*> [1,-1]

-- The 3D faces of a cube.
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

-- Cube face vertices paired with normal vectors.
pts :: [PlainRec [Pos,Normal]]
pts = fold [ map (setNorm z)    front
           , map (setNorm $ -z) back
           , map (setNorm $ -x) left
           , map (setNorm x)    right
           , map (setNorm y)    top
           , map (setNorm $ -y) bottom ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Color the front vertices a dark blue, the back a light beige.
colorize :: PlainRec [Pos,Normal] -> PlainRec [Pos,Normal,Color]
colorize pt = pt <+> col =: c
  where c | view (rLens pos._z) pt > 0 = V3 8.235294e-2 0.20392157 0.3137255
          | otherwise = V3 0.95686275 0.8392157 0.7372549

-- Indices into the vertex array for each face.
inds :: [Word32]
inds = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
  where faceInds = [0,1,2,2,1,3]

-- For rendering a cube, we'll need a ModelView matrix, and a
-- ProjectionModelView matrix.
type CamInfo = PlainRec ["cam" ::: M44 GLfloat, "proj" ::: M44 GLfloat]

cube :: (i <: CamInfo) => IO (i -> IO ())
cube = do s <- simpleShaderProgram ("etc"</>"poly.vert") ("etc"</>"poly.frag")
          vb <- bufferVertices (map colorize pts)
          eb <- makeBuffer ElementArrayBuffer inds
          vao <- makeVAO $
                 do currentProgram $= Just (program s)
                    setUniforms s (light =: normalize (V3 0 0 1))
                    enableVertices' s vb
                    bindVertices vb
                    bindBuffer ElementArrayBuffer $= Just eb
          let ss = setUniforms s
          return $ \appInfo -> withVAO vao $
            do currentProgram $= Just (program s)
               ss (cast appInfo :: CamInfo)
               drawIndexedTris 12
  where light :: "lightDir" ::: V3 GLfloat
        light = Field

-- We don't use normal vectors with the ground, so we just need a
-- single composite projection matrix.
type ProjInfo = PlainRec '["proj" ::: M44 GLfloat]

-- Ground texture from:
-- http://www.texturehd.com/data/media/21/Wood_floor_boards.jpg
ground :: (i <: ProjInfo) => IO (i -> IO ())
ground = do Right t <- readTexture $ "art"</>"Wood_floor_boards.png"
            generateMipmap' Texture2D
            s <- simpleShaderProgram ("etc"</>"ground.vert") ("etc"</>"ground.frag")
            vb <- bufferVertices . map ((pos =:) . scale3D) $
                  V2 <$> [-1,1] <*> [-1,1]
            vao <- makeVAO $
                   do currentProgram $= Just (program s)
                      enableVertices' s vb
                      bindVertices vb
                      setUniforms s (tex =: 0)
                      textureBinding Texture2D $= Just t
                      textureFilter Texture2D $= 
                        ((Linear', Just Linear'), Linear')
                      texture2DWrap $= (Repeated, Repeat)
            let ss = setUniforms s
            return $ \appInfo -> withVAO vao $
              do currentProgram $= Just (program s)
                 ss (cast appInfo :: ProjInfo)
                 withTextures2D [t] $ drawArrays TriangleStrip 0 4
  where scale3D :: V2 GLfloat -> V3 GLfloat
        scale3D = (\(V2 x z) -> V3 x (-1.01) z) . (3*^)
        tex :: "tex" ::: GLint
        tex = Field
