This article is for those familiar with
[Haskell](http://www.haskell.org/haskellwiki/Haskell) and, at least
passingly, with Jon Sterling's
[`vinyl`](http://hackage.haskell.org/package/vinyl) library (that
packs quite a nice
[introduction](http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html)). Further,
it is assumed that the reader is familiar with the basics of computer
graphics and [OpenGL](http://www.opengl.org). I have written another
[article](http://www.arcadianvisions.com/blog/?p=224) that introduces
the use of relatively modern OpenGL practice with Haskell that may
serve as a primer for this article.

This post is a guided tour through the [Game2D
example](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Game2D.hs)
in the [vinyl-gl repository](https://github.com/acowley/vinyl-gl). I
think (hope) the
[examples](https://github.com/acowley/vinyl-gl/tree/master/examples)
directory there is a useful starting point to see how this library may
be used. If you want to skip the examples and just get coding, the
library is [available on
hackage](http://hackage.haskell.org/package/vinyl-gl).

What's the Problem?
---

A drawback to the changes undergone by [OpenGL](http://www.opengl.org)
over the past few years as it has shifted from a fixed-function to a
programmable pipeline is that it now seems much harder to get
started. Part of the problem is that, while moving virtually all
processing into shader programs provides better performance and vastly
more flexibility, programmers must now connect their application code
to their GLSL code. This is a bit of a pain as it typically requires
thinking about detailed aspects of memory layout, and rather pedantic
interactions with OpenGL in order to get past the fact that you must
hand it a `void*` with the promise that this blob of memory is full of
useful goodies.

A Worked Example
---

A first example of connecting Haskell OpenGL code with GLSL is the
skeleton of a 2D side-scroller game (think *Super Mario Bros.*). In
these games, levels are often drawn as a set of reusable tiles, and
player movement causes a camera to translate across the level. Here's
what it will look like,

![](../images/vinyl-gl-game2d.png "vinyl-gl Game2D Screenshot")

I will begin by presenting the GLSL code to be used. I am going to be
drawing textured 2D triangles to make up the game level. Our vertex
shader will take in a 3x3 matrix that captures the 2D transformations
our geometry may undergo. A vertex coordinate and a texture coordinate
will comprise the vertex input for the shader, which will then output
clip-space coordinates for the vertex data (`gl_Position`), and
pass the texture coordinates through to the fragment shader.

```C
 #version 150
uniform mat3 cam;
in vec2 vertexCoord;
in vec2 texCoord;
out vec2 texCoordFrag;

void main() {
  texCoordFrag = texCoord;
  gl_Position = vec4(cam * (vec3(vertexCoord, 1) * 2 - 1), 1);
}
```

The fragment shader simply samples the texture.

```C
 #version 150
uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 fragColor;

void main() {
  fragColor = texture(tex, texCoordFrag);
}
```

In summary, we have uniform inputs `cam` and `tex`. We also have
per-vertex inputs `vertexCoord` and `texCoord`.

Now we can write some Haskell to feed the GLSL monster. We start by
pulling in [`vinyl`](http://hackage.haskell.org/package/vinyl),
[`GLUtil`](http://hackage.haskell.org/package/GLUtil), and
[`vinyl-gl`](https://github.com/acowley/vinyl-gl) in addition to some
of the usual suspects.

> {-# LANGUAGE DataKinds, TypeOperators #-}
> import Control.Applicative
> import Data.Foldable (foldMap, traverse_)
> import Data.Vinyl
> import Graphics.GLUtil
> import Graphics.GLUtil.Camera2D
> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLFW (Key(KeyEsc))
> import Graphics.VinylGL
> import Linear (V2(..), _x, M33)
> import System.FilePath ((</>))

We also have some helper modules used in these examples. These modules
are tied to the [`GLFW-b`](http://hackage.haskell.org/package/GLFW-b)
package's windowing and input facilities. They serve to kick off our
main window with an event loop, and map keyboard input to camera
movements.

> import Keyboard2D (moveCamera)
> import Window (initGL, UI(..))

The rendering parts of our application need to refer to some common
rendering state. In our case, we just have a 2D transformation matrix
that operates on homogenous 2D points, but we use a `vinyl` record to
get used to techniques that let us avoid coupling the rendering state
across all rendering functions. Take a look `Demo3D.hs` example and
its associated rendering functions in `Geometry.hs` to see this in
action. Note the association between the field named `"cam"` and the
vertex shader uniform named `cam`.

> type AppInfo = PlainRec '["cam" ::: M33 GLfloat]

Our game is just going to have ground in front of a blue sky
background. The ground is made up of columns of dirt tiles, each
topped by a grass tile. Given that design, a level in our game is
simply a list of column heights. Here we work with game maps made up
of columns whose height ranges from 0 to 10.

> gameLevel :: [Int]
> gameLevel = [3,3,3,4,5,4,3,3,3,4,5,5,6,7,6,6,6,7,6,5,4,3,3]

We will need to convert a tile height to a 2D square in order to draw
the tile on the screen. We define this mapping once so we have a
compact game level representation and a consistent-by-construction
tile size. Since we are only given the height of the tile to produce,
we set its left edge to have an X coordinate of 0. We also here
convert our game level's vertical coordinate system of [0,10] to
normalized coordinates that range from 0 to 1. The `tile` function
gives us the four vertices of a square for a tile.

> tile :: Int -> [V2 GLfloat]
> tile h = let h' = fromIntegral h / 10 in V2 <$> [0,0.2] <*> [h', h' - 0.2]

The `tile` function can be used to build a column of tiles whose left
edge has an X coordinate of 0. We now lay these columns out
side-by-side extending from a leftmost edge at the Y axis. Each
successive column is placed farther along the positive X axis.

> spaceColumns :: [[V2 GLfloat]] -> [[V2 GLfloat]]
> spaceColumns = zipWith (map . (_x +~)) [0, 0.2 ..]

Building a Bridge to GLSL
---

The 2D vertices we produce will consist of not only 2D positions, but
also texture coordinates that define how our tile images are mapped to
2D tiles. Having this `Tex` field means that we have the flexibility
to switch to a sprite atlas technique, or to repeat an image across a
tile. Once again, we are arranging a coincidence of naming here: the
vinyl `Field` `pos` has a type with name `"vertexCoord"` that matches
up with the `vertexCoord` our vertex shader expects as input.

> type Pos = "vertexCoord" ::: V2 GLfloat
> type Tex = "texCoord"    ::: V2 GLfloat
> 
> pos :: Pos
> pos = Field
> 
> tex :: Tex
> tex = Field

We're not going to do anything fancy with texture coordinates for now,
so we will simply assign each vertex texture coordinates that map the
entire texture to the tile square.

> tileTex :: [[V2 GLfloat]] -> [PlainRec [Pos,Tex]]
> tileTex = foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
>   where coords = map (tex =:) $ V2 <$> [0,1] <*> [0,1]

We compute all the grassy tiles from our `gameLevel` data structure
together because they share the same texture. By grouping our tiles by
their textures, we can simplify eventual rendering.

The `grassTiles` action computes a list of vertices with position
(`Pos`) and texture coordinate (`Tex`) fields, then loads all that
data into OpenGL. The fields used here will correspond to the inputs
expected by our GLSL vertex shader. Data is loaded into OpenGL with
the `bufferVertices` function that takes a list (or a
`Data.Vector.Storable.Vector`) of `PlainRec`s and feeds the data into
OpenGL. Note that the `BufferedVertices` type is tagged with the
fields of the buffered vertex data. This lets us compare our buffered
data with the expectations of our GLSL program when possible.

> grassTiles :: IO (BufferedVertices [Pos,Tex])
> grassTiles = bufferVertices . tileTex . spaceColumns $ map tile gameLevel

Producing the columns of dirt that support our grassy tiles is
slightly more complicated. Conceptually, we generate a new tile at
every possible tile height down to 1 (remember our game level has
tiles going up from 0). This means that rather than producing one tile
for each element of the `gameLevel` structure, we instead produce a
list of tiles corresponding to a dirt column in our game world.

> dirtTiles :: IO (BufferedVertices [Pos,Tex])
> dirtTiles = bufferVertices . tileTex . spaceColumns $ map col gameLevel
>   where col :: Int -> [V2 GLfloat]
>         col h = foldMap tile [h - 1, h - 2 .. 1]

So much for geometry! We have now defined how to produce a bunch of
textured 2D squares from our `gameLevel` data structure. The next step
is to load the images we want to use into OpenGL
`TextureObject`s. Here, we load images from the `art` directory, and
set each to use nearest-neighbor filtering.

> loadTextures :: [FilePath] -> IO [TextureObject]
> loadTextures = fmap (either error id . sequence) . mapM aux
>   where aux f = do img <- readTexture ("art" </> f)
>                    traverse_ (const texFilter) img
>                    return img
>         texFilter = do textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
>                        texture2DWrap $= (Repeated, ClampToEdge)

Ready to Renderer
---

Finally, we can define how to draw our game world! We will make use of
the geometry and texture loading pieces defined above to produce a
function that will draw our game level. I obtained the images used
here from
[OpenGameArt.org](http://opengameart.org/content/platformer-tiles);
with thanks to the artist, Kenney.nl.

> background :: IO (AppInfo -> IO ())
> background = 
>   do [grass,dirt] <- loadTextures [ "ground.png", "ground_dirt.png" ]

With our textures loaded, we are now going to load our GLSL program by
specifying a vertex shader and a fragment shader.

>      s <- loadShaderProgram ("etc"</>"game2d.vert") ("etc"</>"game2d.frag")

Our fragment shader has a `sampler2D` uniform input to support
texturing. We are always going to use the first texture unit, denoted
0, so we can set this uniform parameter of our shader program now.

>      setUniforms s (texSampler =: 0)

We are now ready to prepare our geometry. Recall that we have code
that loads a bunch of 2D vertices into OpenGL: four vertices for each
tile. We are going to render each tile as a pair of triangles, but we
don't want to make separate rendering calls for every tile, or even
every triangle. To this end, we will also provide OpenGL with vertex
indices that describe how to index into the buffered vertices to
produce triangles.

We have defined our square tile vertices like this:

~~~~
0 ------- 2
|         |
|         |
|         |
1---------3
~~~~

These four vertices may be drawn as two triangles with
counterclockwise winding by considering one triangle consisting of
vertices `[0,1,2]` and another consisting of vertices `[2,1,3]`. Thus
the indices `[0,1,2,2,1,3]` may be used to index into a buffer of four
vertices to render two triangles making up one square tile. We've
buffered all our tile vertices contiguously, so all we need to do is
repeat the `[0,1,2,2,1,3]` pattern for each tile, shifting the
sequence by 4 for each tile (for example, the second tile indexes into
the buffered vertex data at `[4,5,6,6,5,7]`). We build an infinite
list of such shifted sequences in the definition for `inds` below,
then `take` 6 indices for each tile in our game world.

>      grassVerts <- grassTiles
>      eb <- bufferIndices inds
>      grassVAO <- makeVAO $ do enableVertices' s grassVerts
>                               bindVertices grassVerts
>                               bindBuffer ElementArrayBuffer $= Just eb
>      dirtVerts <- dirtTiles

OpenGL is a stateful system, and Vertex Array Objects (VAOs) were
added to help encapsulate some of this state. In particular, we need
to bind each field of our vertices to its corresponding GLSL shader
input. We do this by giving OpenGL offsets and strides into our vertex
buffer that define how to pick out each field of each vertex. The
`vinyl-gl` library handles all of this automatically. The convention
that drives the entire arrangement is that the symbolic name attached
to each field is identical to the corresponding GLSL input's name.

>      dirtVAO <- makeVAO $ do enableVertices' s dirtVerts
>                              bindVertices dirtVerts
>                              bindBuffer ElementArrayBuffer $= Just eb
>      return $ \i -> do currentProgram $= Just (program s)
>                        setUniforms s i
>                        withVAO grassVAO . withTextures2D [grass] $
>                          drawIndexedTris numGrassTris
>                        withVAO dirtVAO . withTextures2D [dirt] $
>                          drawIndexedTris numDirtTris
>   where numGrassTris = fromIntegral $ 2 * length gameLevel
>         numDirtTris = fromIntegral . sum $ map (*2) gameLevel
>         texSampler = Field :: "tex" ::: GLint
>         inds = take (sum $ map (*6) gameLevel) $
>                foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]

The Window and Main Loop
---

Our application is driven by a single initialization function. We set
the color to which each frame will be cleared, turn on alpha blending,
as our textures have alpha channels, and initialize our background
renderer.

> setup :: IO (AppInfo -> IO ())
> setup = do clearColor $= Color4 0.812 0.957 0.969 1
>            blend $= Enabled
>            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
>            background

The game loop makes use of a frame-tick action that provides an
updated `UI` value and the drawing function returned from `setup`.

> loop :: IO UI -> IO ()
> loop tick = setup >>= go camera2D
>   where go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
>         go c draw = 
>           do ui <- tick
>              clear [ColorBuffer, DepthBuffer]
>              let mCam = camMatrix c
>                  info = Field =: mCam
>              draw info
>              if keysPressed ui ^. contains KeyEsc
>              then return () -- terminate
>              else go (moveCamera ui c) draw

Open the window and kick off the loop!

> main :: IO ()
> main = usage >> initGL "2D Platformer" 640 480 >>= loop

> usage :: IO ()
> usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"

And that's it! Since walking through working code isn't entirely
satisfying, what would happen if we screwed up the synchronization
between Haskell and GLSL?

Safety Net
---

Suppose we had written:

```Haskell
type Pos = "vertexCoord" ::: V2 GLint
```

and treated vertex position as an integer value throughout.

This would result in a runtime error at startup:

```
Game2D: Type mismatch in vertexCoord
```

Similarly, had we written:

```Haskell
type Pos = "vertexCoord" ::: V3 GLfloat
```

We would see the same error at runtime. We are not inspecting the GLSL
program at compile time, as it may change between the time we
typecheck our Haskell program and run our executable. However, once we
have loaded GLSL code at runtime, and tried to enable some
`BufferedVertices` with `enableVertices'`, `vinyl-gl` can detect
mismatches between the Haskell and GLSL sides.

Another potential mishap: had we let our names get out of sync and
written,

```Haskell
type Pos = "vCoords" ::: V2 GLfloat
```

We would get a different error at runtime:

```
Game2D: GLSL expecting vertexCoord
```

These hypothetical errors may seem a bit contrived, but easily arise
when composing larger applications from small pieces, as we always do
in Haskell. The extensible records provided by `vinyl` combined with
the `vinyl-gl` machinery let us,

* Append fields to records to grow complex vertex descriptors
* Project out simpler supertypes from more richly defined descriptors
* Load complex vertex data into OpenGL, but only map parts of the
data to GLSL parameters

This flexibility helps the programmer avoid getting boxed in by
specific record types while maintaining good relations between Haskell
and GLSL code.

Short and Sweet
---

To bring things full circle, here is a version of the
[code](https://github.com/acowley/GLUtil/blob/master/examples/example1.hs)
from the older [article](http://www.arcadianvisions.com/blog/?p=224)
linked at the top compared to a
[version](https://github.com/acowley/vinyl-gl/blob/master/examples/src/example1-vinyl.hs)
ported to use `vinyl-gl` and the newest `GLUtil`. We have eliminated
nearly *half* the code by reducing OpenGL boilerplate (GLSL &harr;
Haskell synchronization, `VertexArrayDescriptor` specifications,
etc.), and better encapsulating rendering actions.

(Old version on the left, new version on the right.)

[![](../images/vinyl-gl-comparison.png "vinyl-gl code comparison")](../images/vinyl-gl-comparison.pdf)
