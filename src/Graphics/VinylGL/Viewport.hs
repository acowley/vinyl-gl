{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
module Graphics.VinylGL.Viewport (Viewport, withVinylViewport) where
import Data.Vinyl
import Graphics.Rendering.OpenGL
import Graphics.GLUtil (withViewport)
import Linear (V2(..))

getSubField :: forall r sy t. (r <: PlainRec '[sy:::t]) => (sy:::t) -> r -> t
getSubField f = (rGet f :: PlainRec '[sy:::t] -> t) . cast

type Viewport = "viewport" ::: V2 Int

-- | Uses a field of type @"viewport":::V2 Int@ from a record to run
-- the given action with the specified viewport anchored at the given
-- 'Position'.
withVinylViewport :: (r <: PlainRec '[Viewport])
                  => Position -> r -> IO b -> IO b
withVinylViewport p = withViewport p . margin . vSize 
                    . fmap fromIntegral
                    . getSubField (Field::Viewport)
  where margin (Size w h) = let Position x y = p in Size (w - x) (h - y)
        vSize (V2 w h) = Size w h
