{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Engine.Update where

import Apecs (cmap, cmapM_, global, liftIO, set)
import Control.Monad (when)
import Engine.Buildings.Core (updateBuildings)
import Engine.Components
  ( DrawLevel (..),
    MousePosition (MousePosition),
    Position (Position),
    Velocity (Velocity),
  )
import Engine.DataTypes (DrawLevels (..))
import Engine.Particles (spawnParticles, updateParticlePositions, updateParticles)
import Engine.UI (updateUI)
import Engine.Utils (gget)
import Engine.Villagers.Core (updateVillagers)
import Engine.World (System')
import Foreign.C.Types ()
import Linear (V2 (..))
import qualified SDL
import System.Random (StdGen)

step :: StdGen -> Float -> System' ()
step rng delta = do
  drawLevel <- gget @DrawLevel
  (SDL.P intMousePos) <- SDL.getAbsoluteMouseLocation
  let mousePos = fromIntegral <$> intMousePos
  set global $ MousePosition mousePos
  when (drawLevel == DrawLevel All || drawLevel == DrawLevel Particles) $ spawnParticles 1
  updateVillagers delta
  updateBuildings delta
  updateParticles delta
  updateParticlePositions delta
  updateUI delta

moveCharacters :: Float -> System' ()
moveCharacters dT =
  cmap $ \(Position pos, Velocity vel) -> Position $ pos + ((* dT) <$> vel)

printPositions :: System' ()
printPositions = do
  cmapM_ (\(Position (V2 x y)) -> liftIO $ putStrLn (show x ++ " " ++ show y))
