{-# LANGUAGE DataKinds, TypeOperators #-}
import Control.Applicative
import Control.Lens ((^.), contains)
import Data.Vinyl
import Graphics.UI.GLFW (Key(Key'Escape))
import Linear (V2(..), V3(..), M44, (!*!))
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import System.FilePath ((</>))

import Geometry (ground, cube)
import Keyboard3D (moveCamera)
import Graphics.VinylGL
import Window (initGL, UI(..))

-- Note that the field name, "vertexCoord", matches the attribute name
-- in the vertex shader.
pos :: SField '("vertexCoord", v GLfloat)
pos = SField

tex :: SField '("tex", GLint)
tex = SField

logo :: IO (IO ())
logo = do Right t <- readTexture ("art"</>"Haskell-Logo.png")
          s <- simpleShaderProgram ("etc"</>"logo.vert") ("etc"</>"logo.frag")
          vb <- bufferVertices $ map (pos =:) [0, V2 0.25 0, 0.25, V2 0 0.25]
          vao <- makeVAO $
                 do currentProgram $= Just (program s)
                    enableVertices' s vb
                    bindVertices vb
                    textureBinding Texture2D $= Just t
                    textureFilter Texture2D $= 
                      ((Nearest, Nothing), Nearest)
                    texture2DWrap $= (Mirrored, ClampToEdge)
                    setUniforms s (tex =: 0)
          return . withVAO vao $ 
            do currentProgram $= Just (program s)
               withTextures2D [t] (drawArrays TriangleFan 0 4)

type Viewport = '("viewport", V2 GLsizei)

type AppInfo = FieldRec [ '("cam", M44 GLfloat)
                        , '("proj", M44 GLfloat)
                        , Viewport ]

setup :: IO (AppInfo -> IO ())
setup = do clearColor $= Color4 0.3 0.6 0.3 1
           depthFunc $= Just Lequal
           blend $= Enabled
           blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
           mainView <- ((sequence_ .) . sequence) <$> sequence [ground, cube]
           subView <- logo
           return $ \x -> subView >> vp x (mainView x)
  where vp = withViewport (Position px py)
           . (\(V2 w h) -> Size w h) . (subtract vPos)
           . getField . rget (SField::SField Viewport)
        vPos@(V2 px py) = V2 160 120

loop :: IO UI -> IO ()
loop tick = setup >>= go cam0
  where go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
        go c draw = 
          do ui <- tick
             clear [ColorBuffer, DepthBuffer]
             let V2 ww wh = fromIntegral <$> (windowSize ui - V2 160 120)
                 mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
                 mCam = camMatrix c
                 info =  SField =: mCam
                     <+> SField =: (mProj !*! mCam)
                     <+> SField =: (fromIntegral <$> windowSize ui)
             draw info
             if keysPressed ui ^. contains Key'Escape
             then return () -- terminate
             else go (moveCamera ui c) draw
        cam0 = tilt (-20) $ dolly (V3 0 2 8) fpsCamera

main :: IO ()
main = usage >> initGL "Easy Render" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
