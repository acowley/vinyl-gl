{-# LANGUAGE DataKinds, TypeOperators #-}
import Control.Applicative
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Key(KeyEsc))
import Graphics.VinylGL
import Linear (V2(..), _x, M33)
import System.FilePath ((</>))
import Keyboard2D (moveCamera)
import Window (initGL, UI(..))

-- A record each drawing function will receive.
type AppInfo = PlainRec '["cam" ::: M33 GLfloat]

-- The ground level for each column in our map. Heights go up from 0
-- to 10.
gameLevel :: [Int]
gameLevel = [3,3,3,4,5,4,3,3,3,4,5,5,6,7,6,6,6,7,6,5,4,3,3]

-- Produce square vertices in normalized ([0,1]) coordsinates for a
-- tile whose top is at the given height.
tile :: Int -> [V2 GLfloat]
tile h = let h' = fromIntegral h / 10 in V2 <$> [0,0.2] <*> [h', h' - 0.2]

-- Our vertices will have position and texture coordinates.
type Pos = "vertexCoord" ::: V2 GLfloat
type Tex = "texCoord"    ::: V2 GLfloat

pos :: Pos
pos = Field

tex :: Tex
tex = Field

-- Each element of the outer list is a list of the vertices that make
-- up a column. Push each successive column farther along the X axis.
spaceColumns :: [[V2 GLfloat]] -> [[V2 GLfloat]]
spaceColumns = zipWith (map . (_x +~)) [0, 0.2 ..]

-- Compute a textured vertex record for each input vertex.
tileTex :: [[V2 GLfloat]] -> [PlainRec [Pos,Tex]]
tileTex = foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
  where coords = map (tex =:) $ V2 <$> [0,1] <*> [0,1]

-- Load the geometry data for all grass tiles into OpenGL.
grassTiles :: IO (BufferedVertices [Pos,Tex])
grassTiles = bufferVertices . tileTex . spaceColumns $ map tile gameLevel

-- Load the geometry data for all dirt tiles into OpenGL.
dirtTiles :: IO (BufferedVertices [Pos,Tex])
dirtTiles = bufferVertices . tileTex . spaceColumns $ map col gameLevel
  where col :: Int -> [V2 GLfloat]
        col h = foldMap tile [h - 1, h - 2 .. 1]

-- Load a list of textures from the art directory. Set each texture to
-- use nearest-neighbor filtering.
loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do img <- readTexture ("art" </> f)
                   traverse_ (const texFilter) img
                   return img
        texFilter = do textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
                       texture2DWrap $= (Repeated, ClampToEdge)

-- Ground textures from: http://opengameart.org/content/platformer-tiles
-- Attributed to: "Kenney.nl" or "www.kenney.nl"
background :: IO (AppInfo -> IO ())
background = 
  do [grass,dirt] <- loadTextures [ "ground.png", "ground_dirt.png" ]
     s <- loadShaderProgram ("etc"</>"game2d.vert") ("etc"</>"game2d.frag")
     setUniforms s (texSampler =: 0)
     grassVerts <- grassTiles
     eb <- bufferIndices inds
     grassVAO <- makeVAO $ do enableVertices' s grassVerts
                              bindVertices grassVerts
                              bindBuffer ElementArrayBuffer $= Just eb
     dirtVerts <- dirtTiles
     dirtVAO <- makeVAO $ do enableVertices' s dirtVerts
                             bindVertices dirtVerts
                             bindBuffer ElementArrayBuffer $= Just eb
     return $ \i -> do currentProgram $= Just (program s)
                       setUniforms s i
                       withVAO grassVAO . withTextures2D [grass] $
                         drawIndexedTris numGrassTris
                       withVAO dirtVAO . withTextures2D [dirt] $
                         drawIndexedTris numDirtTris
  where numGrassTris = fromIntegral $ 2 * length gameLevel
        numDirtTris = fromIntegral . sum $ map (*2) gameLevel
        texSampler = Field :: "tex" ::: GLint
        inds = take (sum $ map (*6) gameLevel) $
               foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]

-- Initialize OpenGL and our background-drawing function.
setup :: IO (AppInfo -> IO ())
setup = do clearColor $= Color4 0.812 0.957 0.969 1
           blend $= Enabled
           blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
           background

-- The game loop makes use of a frame-tick action that provides an
-- updated 'UI' value and the drawing function returned from 'setup'.
loop :: IO UI -> IO ()
loop tick = setup >>= go cam0
  where go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
        go c draw = 
          do ui <- tick
             clear [ColorBuffer, DepthBuffer]
             let mCam = camMatrix c
                 info = Field =: mCam
             draw info
             if keysPressed ui ^. contains KeyEsc
             then return () -- terminate
             else go (moveCamera ui c) draw
        cam0 = camera2D

-- Open the window and kick off the loop!
main :: IO ()
main = usage >> initGL "2D Platformer" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
