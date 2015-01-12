{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
import Data.Vinyl
import Data.Word
import Foreign.Ptr (nullPtr, plusPtr)
import Graphics.Rendering.OpenGL (DataType(..), GLfloat,
                                  VertexArrayDescriptor(..))
import Graphics.VinylGL
import Linear (V1(..), V3(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), (~=?))

type Pos = '("vpos", V3 GLfloat)
type Tag = '("tagByte", V1 Word8)

tag :: SField Tag
tag = SField

type Vertex = FieldRec [Pos, Tag]

testVad :: Test
testVad = TestLabel "Sample VAD Creation" $
          vad ~=? fieldToVAD tag (SField::SField Vertex)
  where vad = VertexArrayDescriptor 1 UnsignedByte 13 (nullPtr `plusPtr` 12)

main :: IO ()
main = defaultMain . hUnitTestToTests $ testVad



