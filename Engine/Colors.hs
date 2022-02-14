module Engine.Colors(
    black, blackF, blackP, blackPA, whitePA, blackPA2, brownA,
    background, backgroundA, white, red
) where
import qualified SDL.Font
import qualified SDL
import qualified SDL.Primitive
import Linear (V4(V4))
import qualified Data.Word as GHC.Word
import GHC.Word (Word8)

red :: SDL.V3 Word8
red = SDL.V3 255 0 0

background :: SDL.V3 Word8
background = SDL.V3 116 210 102

backgroundA :: V4 Word8
backgroundA = V4 116 210 102 255

black :: SDL.V3 Word8
black = SDL.V3 0 0 0

blackF :: SDL.Font.Color
blackF = SDL.V4 0 0 0 255

blackP :: SDL.Primitive.Color
blackP = V4 0 0 0 255

blackPA :: SDL.Primitive.Color
blackPA = V4 0 0 0 178

blackPA2 :: SDL.Primitive.Color
blackPA2 = V4 0 0 0 100

white :: SDL.V3 Word8
white = SDL.V3 255 255 255

whitePA :: SDL.Primitive.Color
whitePA = V4 255 255 255 100

brownA :: SDL.Primitive.Color
brownA = V4 100 60 2 200
