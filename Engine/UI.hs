{-# LANGUAGE TypeApplications #-}
module Engine.UI (spawnButton, stepUI) where
import Linear (V2)
import Engine.World (System')
import Engine.Components
import Engine.Utils ((<#>), gget)
import Engine.Constants (defaultRectSize, tileSize)
import Linear.V2 (V2(V2))
import Apecs (newEntity)
import Foreign.C (CInt)
import Engine.DataTypes (ClickState(..), StructureState (Placement))
import Control.Monad.IO.Class
import Apecs.System
import Engine.Buildings (spawnHouse)
import Apecs.Util (global)
import Apecs.Core (Entity(Entity))

stepUI :: Float -> System' ()
stepUI dT = do
  houseButtonClicked

spawnButton :: V2 Float -> (CInt, CInt) -> V2 Float -> Float ->  System' ()
spawnButton pos (tx, ty) v scale = do
  newEntity (
      HouseButton,
      Button NotClicked False False,
      InterfaceBox $ (* scale) <$> v,
      Sprite (V2 (tx * tileSize) (ty * tileSize)) defaultRectSize scale,
      InteractionBox pos (fromIntegral <#> ((* round scale) <$> defaultRectSize)),
      Position pos)
  return ()


houseButtonClicked :: System' ()
houseButtonClicked = cmapM_ $
  \(HouseButton, Button cState _ _) -> 
    case cState of
      Clicked -> do
        MousePosition mPos <- gget @MousePosition
        SelectedConstruction mEty <- gget @SelectedConstruction
        case mEty of
          Nothing -> do
            ety <- spawnHouse mPos Placement 
            set global $ SelectedConstruction $ Just ety
          Just _ -> return ()
      _ -> return ()
