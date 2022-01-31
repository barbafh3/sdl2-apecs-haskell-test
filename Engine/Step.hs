{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Engine.Step (step, gameLoop, moveCharacters, printPositions, filterPayload, handleEvent) where

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
import Engine.Particles (spawnParticles, stepParticles, stepParticlePositions)
import Engine.Villagers (updateVillagers)
import Engine.Buildings (updateBuildings)
import Engine.UI (stepUI)

step :: StdGen -> Float -> System' ()
step rng dT = do
  drawLevel <- gget @DrawLevel
  (SDL.P imPos) <- SDL.getAbsoluteMouseLocation
  let mPos = fromIntegral <$> imPos
  set global $ MousePosition mPos
  when (drawLevel == DrawLevel All || drawLevel == DrawLevel Particles) $ spawnParticles 1
  updateVillagers dT
  updateBuildings dT
  stepParticles dT
  stepParticlePositions dT
  stepUI dT

gameLoop :: Double -> [SDL.EventPayload] -> System' ()
gameLoop delta payload = do
  handlePlayerInput payload delta

moveCharacters :: Float -> System' ()
moveCharacters dT =
  cmap $ \(Position pos, Velocity vel) -> Position $ pos + ((* dT) <$> vel)

printPositions :: System' ()
printPositions = do
  cmapM_ (\(Position (V2 x y)) -> liftIO $ putStrLn (show x ++ " " ++ show y))


handlePlayerInput :: [SDL.EventPayload] -> Double -> System' ()
handlePlayerInput [] _ = return ()
handlePlayerInput [event] delta = filterPayload event
handlePlayerInput (event:events) delta = do
  filterPayload event
  handlePlayerInput events delta

filterPayload :: SDL.EventPayload -> System' ()
filterPayload (SDL.KeyboardEvent k) = handleEvent k
filterPayload _ = return ()
  
handleEvent :: SDL.KeyboardEventData -> System' ()
handleEvent (SDL.KeyboardEventData _ _ _ keysym) = return ()
  -- let key = SDL.keysymKeycode keysym
  -- cmap $ \(Position (V2 x y), Velocity (V2 vx vy)) -> do
  --   Position $ filterMovementKeys key (round vx) $ V2 x y

filterMovementKeys :: SDL.Keycode -> CInt -> V2 CInt -> V2 CInt
filterMovementKeys keycode speed (V2 x y) = 0
  -- case keycode of
  --   SDL.KeycodeW -> V2 x (y - speed)
  --   SDL.KeycodeS -> V2 x (y + speed)
  --   SDL.KeycodeA -> V2 (x - speed) y
  --   SDL.KeycodeD -> V2 (x + speed) y
  --   _ -> V2 x y
