{-# LANGUAGE TypeApplications #-}

module Engine.UI (spawnButton, updateUI) where

import Apecs (newEntity)
import Apecs.Core (Entity (Entity))
import Apecs.System (cmapM_, set)
import Apecs.Util (global)
import Control.Monad.IO.Class ()
import Engine.Buildings.Spawn (spawnHouseConstruction)
import Engine.Components
  ( Button (Button),
    HouseButton (HouseButton),
    InteractionBox (InteractionBox),
    InterfaceBox (InterfaceBox),
    MousePosition (..),
    Position (Position),
    SelectedConstruction (..),
    Sprite (Sprite),
  )
import Engine.Constants (defaultRectSize, tileSize)
import Engine.DataTypes (ClickState (..), StructureState (Placement))
import Engine.Utils (gget, (<#>))
import Engine.World (System')
import Foreign.C (CInt)
import Linear (V2)
import Linear.V2 (V2 (V2))

updateUI :: Float -> System' ()
updateUI dT = do
  houseButtonClicked

spawnButton :: V2 Float -> (CInt, CInt) -> V2 Float -> Float -> System' ()
spawnButton pos (tx, ty) v scale = do
  newEntity
    ( HouseButton,
      Button NotClicked False False,
      InterfaceBox $ (* scale) <$> v,
      Sprite (V2 (tx * tileSize) (ty * tileSize)) defaultRectSize scale,
      InteractionBox pos (fromIntegral <#> ((* round scale) <$> defaultRectSize)),
      Position pos
    )
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
            ety <- spawnHouseConstruction mPos
            set global $ SelectedConstruction $ Just ety
          Just _ -> return ()
      _ -> return ()
