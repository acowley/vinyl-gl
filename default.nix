{ mkDerivation, base, containers, GLUtil, HUnit, linear, OpenGL
, stdenv, tagged, test-framework, test-framework-hunit
, transformers, vector, vinyl, GLFW-b
}:
mkDerivation {
  pname = "vinyl-gl";
  version = "0.3.3";
  src = ./.;
  libraryHaskellDepends = [
    base containers GLUtil linear OpenGL tagged transformers vector
    vinyl
  ];
  testHaskellDepends = [
    base HUnit linear OpenGL tagged test-framework test-framework-hunit
    vinyl GLFW-b
  ];
  description = "Utilities for working with OpenGL's GLSL shading language and vinyl records";
  license = stdenv.lib.licenses.bsd3;
}
