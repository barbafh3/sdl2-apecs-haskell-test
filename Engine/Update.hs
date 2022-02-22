{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Engine.Update where

import Apecs
import qualified SDL
import Linear(V2(..))
import Foreign.C.Types

import Engine.World
import Engine.Components
import System.Random (StdGen)
import Engine.Utils (gget)
import Control.Monad (when)
import Engine.DataTypes (DrawLevels(..))
import Engine.Particles (spawnParticles, updateParticles, updateParticlePositions)
import Engine.Villagers.Core (updateVillagers)
import Engine.Buildings.Core (updateBuildings)
import Engine.UI (updateUI)

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
