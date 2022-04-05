{-# LANGUAGE TypeApplications #-}

module Engine.Buildings.Core
  ( updateBuildings,
  )
where

import Apecs (Entity (Entity), cmapM, get, set)
import Apecs.System (cmapM_)
import Control.Monad (when)
import Engine.Buildings.Tasks (runBuildingTask)
import Engine.Collisions (areBoxesColliding)
import Engine.Colors (background, red)
import Engine.Components
  ( BoundingBox (BoundingBox),
    Building (Building),
    InteractionBox (InteractionBox),
    MousePosition (..),
    Position (Position),
    Sprite (Sprite),
  )
import Engine.DataTypes (StructureState (Placement))
import Engine.Utils (gget)
import Engine.World (System', World)
import Linear (V2 (..))
import SDL (Texture, ($=))
import SDL.Video ()

updateBuildings :: Float -> System' ()
updateBuildings dT = do
  runBuildingTask
  followMouseCursor

followMouseCursor :: System' ()
followMouseCursor = do
  cmapM $ \(Building state, building) -> do
    case state of
      Placement -> do
        MousePosition mPos@(V2 mx my) <- gget @MousePosition
        (InteractionBox ibPos ibSize, BoundingBox bbPos bbSize, Sprite _ (V2 sw sh) _) <- get building
        let pos = V2 (mx - (fromIntegral sw / 2)) (my - (fromIntegral sh / 2))
        set building (Position pos, InteractionBox pos ibSize, BoundingBox pos bbSize)
      _ -> return ()
