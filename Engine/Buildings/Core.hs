{-# LANGUAGE TypeApplications           #-}

module Engine.Buildings.Core (
  updateBuildings,
) where

import Engine.Components
import Engine.DataTypes
import Engine.Utils (gget)
import Linear (V2(..))
import Engine.World (System', World)
import Engine.Buildings.Tasks (runBuildingTask)
import SDL (Texture, ($=))
import Engine.Colors (background, red)
import SDL.Video
import Engine.Collisions (areBoxesColliding)
import Control.Monad (when)
import Apecs ( Entity(Entity), set, get, cmapM )
import Apecs.System (cmapM_)

updateBuildings :: Float -> System'()
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


