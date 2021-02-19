{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Apecs
import Control.Monad
import Linear (V2 (..))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Framerate as Frames

import GameSystem
import GameComponents
import GameLoop
import GameRendering


main :: IO ()
main = do
  world <- initWorld
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "App" SDL.defaultWindow
  renderer <- SDL.createRenderer 
                window 
                (-1) 
                SDL.RendererConfig {
                    SDL.rendererType = SDL.AcceleratedRenderer, 
                    SDL.rendererTargetTexture = False
                }
  SDL.showWindow window
  manager <- Frames.manager
  putStrLn "Main: SDL initialized"

  Frames.set manager 60

  tileset <- SDL.Image.loadTexture renderer "tileset.png"
  putStrLn "Main: Tileset loaded"
  runSystem initializeEntities world

  let loop prevTicks secondTick fpsAcc prevFps = do
        ticks <- SDL.ticks

        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            rawDelta = ticks - prevTicks
            doubleDelta = fromIntegral rawDelta :: Double
            delta = (doubleDelta / 1000.0) :: Double
            calcFps = secondTick + rawDelta > 1000
            newFps = if calcFps then fpsAcc + 1 else prevFps
            newFpsAcc = if calcFps then 1 else fpsAcc + 1
            newNextTicks = if calcFps then mod (secondTick + rawDelta) 1000 else secondTick + rawDelta

        runSystem (gameLoop delta payload) world
        runSystem (draw renderer tileset) world

        Frames.delay manager

        unless quit $ loop ticks newNextTicks newFpsAcc newFps

  loop 0 0 0 0

  putStrLn "Main: Cleaning resources..."

  Frames.destroyManager manager
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.quit
  putStrLn "Main: Closing game..."

quitCheck :: System' ()
quitCheck = undefined

initializeEntities :: System' ()
initializeEntities = do
  newEntity (Player, Sprite (V2 0 (16 * 17)) (V2 16 16), Speed 1, Name "Buba", Position $ V2 0 0)
  newEntity (Sprite (V2 16 (16 * 17)) (V2 16 16), Speed 1, Name "Jose", Position $ V2 32 0)
  newEntity (Name "Jose", Position $ V2 0 0)
  liftIO $ putStrLn "Main: Entities created"


initializeTilemap :: System' ()
initializeTilemap = undefined
