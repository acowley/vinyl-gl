Two example programs demonstrating the `vinyl-gl` library.

[Game2D.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Game2D.hs) is the skeleton of a 2D side-scroller game. Background
image times are drawn on screen, and the camera may be translated
side-to-side and up-down to see different parts of the level.

[Demo3D.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Demo3D.hs)
demonstrates 3D graphics (good name, right?) with textures, mipmaps,
shading, lighting, and viewports. Most of the geometry for this demo
is defined in the
[Geometry.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Geometry.hs)
module.

The helper module
[Window.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Window.hs)
wraps up some [GLFW-b](http://hackage.haskell.org/package/GLFW-b)
interaction to provide for a simple main loop for the application
writer. The helpers
[Keyboard2D.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Keyboard2D.hs)
and
[Keyboard3D.hs](https://github.com/acowley/vinyl-gl/blob/master/examples/src/Keyboard3D.hs)
connect keyboard inputs from GLFW to the camera helpers defined in
[GLUtil](http://hackage.haskell.org/package/GLUtil).

**NOTE**: A `.cabal` file has been provided to make dependency installation
easier, but it is not recommended that these demonstration programs be
installed. Instead, run `cabal build` in the `examples` directory,
then run the executables directly from the `dist` directory
`cabal-install` produces.