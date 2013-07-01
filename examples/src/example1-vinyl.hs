{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
-- | A port of the code presented at [Modern OpenGL with
-- Haskell](http://www.arcadianvisions.com/blog/?p=224) to use the
-- vinyl-gl package. The program fades between two images.
import Control.Applicative
import Control.Monad (when)
import Data.Vinyl ((:::)(..), (=:))
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Graphics.GLUtil
import Graphics.VinylGL
import Linear (V1(..), V2(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

-- | The state used by our application.
data Resources = Resources { fadeFactor :: GLfloat }

-- | Load an image file, and set the texturing filter and wrap modes.
loadTex :: FilePath -> IO TextureObject
loadTex f = do t <- either error id <$> readTexture f
               textureFilter Texture2D $= ((Linear', Nothing), Linear')
               texture2DWrap $= (Mirrored, ClampToEdge)
               return t

-- | Loads geometry, shader, and texture resources, then returns a
-- function that draws our scene with the current fade state.
imageFade :: IO (Resources -> IO ())
imageFade = 
  do vb <- bufferVertices . map (pos =:) $ V2 <$> [-1,1] <*> [-1,1]
     s <- loadShaderProgram ("etc"</>"hello-gl.vert") ("etc"</>"hello-gl.frag")
     ts <- mapM (loadTex . ("art"</>)) ["hello1.png", "hello2.png"]
     vao <- makeVAO $ do currentProgram $= Just (program s)
                         setUniforms s (texs =: map V1 [0,1])
                         enableVertices' s vb
                         bindVertices vb
     let setFade = setUniforms s . (fade =:)
     return $ \r ->
       do currentProgram $= Just (program s)
          setFade $ fadeFactor r
          withVAO vao . withTextures2D ts $ drawArrays TriangleStrip 0 4
  where pos = Field :: "position" ::: V2 GLfloat
        texs = Field :: "textures" ::: [V1 GLint]
        fade = Field :: "fade_factor" ::: GLfloat

animate :: Resources -> IO Resources
animate r = do seconds <- getTime
               let fade = sin seconds * 0.5 + 0.5
               return r { fadeFactor = realToFrac fade }

main :: IO ()
main = 
  do currDir <- getCurrentDirectory
     _ <- initialize
     _ <- openWindow opts
     setWindowTitle "Chapter 2 - Vinylized"
     setCurrentDirectory currDir
     draw <- imageFade
     let go r = do draw r
                   swapBuffers
                   pollEvents
                   keyIsPressed KeyEsc >>= flip when (animate r >>= go) . not
     go (Resources 0.0)
  where opts = defaultDisplayOptions { displayOptions_width = 500
                                     , displayOptions_height = 500
                                     , displayOptions_refreshRate = Just 100
                                     , displayOptions_openGLVersion = (3,2) }
