-- | Open a window and get an OpenGL context.
module Window (UI(..), initGL, terminate, closeWindow) where
import Control.Applicative
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Graphics.UI.GLFW
import Linear
import System.Directory (getCurrentDirectory, setCurrentDirectory)

-- | Interface updates provided to the party responsible for
-- generating each frame.
data UI = UI { timeStep       :: Double 
             -- ^ Time in seconds since last frame
             , keysPressed    :: Set Key
             -- ^ All keys currently pressed
             , buttonsPressed :: Set MouseButton
             -- ^ All mouse buttons currently pressed
             , mousePos       :: V2 Int
             -- ^ Current mouse position
             , windowSize     :: V2 Int 
             -- ^ Current window size 
             }

keyCallback :: IORef (Set Key) -> KeyCallback
keyCallback keys k True = modifyIORef' keys (S.insert k)
keyCallback keys k False = modifyIORef' keys (S.delete k)

mbCallback :: IORef (Set MouseButton) -> MouseButtonCallback
mbCallback mbs b True = modifyIORef' mbs (S.insert b)
mbCallback mbs b False = modifyIORef' mbs (S.delete b)

mpCallback :: IORef (V2 Int) -> MousePositionCallback
mpCallback mp x y = writeIORef mp (V2 x y)

wsCallback :: IORef (V2 Int) -> WindowSizeCallback
wsCallback ws w h = writeIORef ws (V2 w h)

-- | @initGL windowTitle width height@ creates a window with the given
-- title and dimensions. The action returned presents a new frame (by
-- performing a buffer swap) and produces an updated snapshot of the
-- user interface.
initGL :: String -> Int -> Int -> IO (IO UI)
initGL windowTitle width height =
  do currDir <- getCurrentDirectory
     _ <- initialize
     _ <- openWindow opts
     setWindowTitle windowTitle
     kbState <- newIORef S.empty
     mbState <- newIORef S.empty
     mpState <- getMousePosition >>= newIORef . uncurry V2
     wsState <- getWindowDimensions >>= newIORef . uncurry V2
     lastTick <- getCurrentTime >>= newIORef
     setKeyCallback (keyCallback kbState)
     setMouseButtonCallback (mbCallback mbState)
     setMousePositionCallback (mpCallback mpState)
     setWindowSizeCallback (wsCallback wsState)
     setCurrentDirectory currDir
     return $ do swapBuffers
                 pollEvents
                 t <- getCurrentTime
                 dt <- realToFrac . diffUTCTime t <$> readIORef lastTick
                 writeIORef lastTick t
                 UI dt <$> readIORef kbState
                       <*> readIORef mbState
                       <*> readIORef mpState
                       <*> readIORef wsState
  where opts = defaultDisplayOptions { displayOptions_width = width
                                     , displayOptions_height = height
                                     , displayOptions_numDepthBits = 24
                                     , displayOptions_openGLVersion = (3,2) }
