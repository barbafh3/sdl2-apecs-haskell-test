module Engine.UI (spawnButton) where
import Linear (V2)
import Engine.World (System')
import Engine.Components (Button(Button), Sprite (Sprite), InteractionBox (InteractionBox), Position (Position), InterfaceBox (InterfaceBox))
import Engine.Utils ((<#>))
import Engine.Constants (defaultRectSize, tileSize)
import Linear.V2 (V2(V2))
import Apecs (newEntity)
import Foreign.C (CInt)

spawnButton :: V2 Float -> (CInt, CInt) -> V2 Float -> Float ->  System' ()
spawnButton pos (tx, ty) v scale = do
  newEntity (
      Button False False False,
      InterfaceBox $ (* scale) <$> v,
      Sprite (V2 (tx * tileSize) (ty * tileSize)) defaultRectSize scale,
      InteractionBox pos (fromIntegral <#> ((* round scale) <$> defaultRectSize)),
      Position pos)
  return ()
