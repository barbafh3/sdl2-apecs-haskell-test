{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Apecs (liftIO, newEntity, runSystem)
import Control.Monad (unless)
import Engine.Buildings.Spawn (spawnFinishedHouse, spawnFinishedStorage)
import Engine.Colors (background)
import Engine.Components
  ( BoundingBox (BoundingBox),
    Building (Building),
    DrawLevel (DrawLevel),
    EntityName (EntityName),
    Fonts (Fonts),
    InfoPanel (InfoPanel),
    InteractionBox (InteractionBox),
    Position (Position),
    Rng (Rng),
    SelectedConstruction (SelectedConstruction),
    Sprite (Sprite),
    UIText (UIText),
  )
import Engine.Constants
  ( defaultRectSize,
    defaultRectSizeV2,
    ptsFontPath,
    pxFontPath,
    screenHeight,
    screenWidth,
    tileSize,
    tilesetPath,
  )
import Engine.DataTypes (DrawLevels (Debug), StructureState (Construction, Enabled))
import Engine.Input (handleInputPayload)
import Engine.Particles (spawnParticles)
import Engine.Rendering (draw)
import Engine.UI (spawnButton)
import Engine.Update (step)
import Engine.Utils (createResourceMap, loadFonts, (<#>))
import Engine.Villagers.Spawn (spawnHauler)
import Engine.World (System', initWorld)
import Linear (V2 (..), V4 (..))
import qualified SDL
import qualified SDL.Font
import qualified SDL.Framerate as Frames
import qualified SDL.Image
import SDL.Video (textureColorMod)
import System.Random (StdGen, mkStdGen, randomIO)

main :: IO ()
main = do
  world <- initWorld
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize
  let winConfig = SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  window <- SDL.createWindow "Protobuilder-HS" winConfig
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer,
          SDL.rendererTargetTexture = False
        }
  SDL.showWindow window
  manager <- Frames.manager
  Frames.set manager 60
  putStrLn "Main: SDL initialized"

  seed <- randomIO
  let rng = mkStdGen seed

  tileset <- SDL.Image.loadTexture renderer tilesetPath
  putStrLn "Main: Tileset loaded"
  runSystem (initializeGame rng) world
  putStrLn "Main: World generated"

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

        runSystem (handleInputPayload payload) world
        runSystem (step rng $ realToFrac delta) world
        runSystem (draw renderer tileset newFps) world

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

initializeGame :: StdGen -> System' ()
initializeGame rng = do
  spawnHauler (V2 680 300) (V2 640 450) (V2 100 100)
  spawnHauler (V2 600 100) (V2 640 450) (V2 100 100)
  spawnFinishedHouse (V2 1000 200)
  newEntity
    ( Building Enabled,
      EntityName "Idle Point",
      Sprite (V2 (2 * tileSize) (6 * tileSize)) defaultRectSize 1,
      BoundingBox (V2 640 450) (V2 8 8),
      InteractionBox (V2 640 450) defaultRectSizeV2,
      Position $ V2 640 450
    )
  spawnFinishedStorage (V2 300 500) [("Wood", 100)] Enabled
  spawnButton (V2 50 800) (1, 2) (V2 24 24) 2
  newEntity $ Rng rng
  newEntity $ DrawLevel Debug
  newEntity $ InfoPanel Nothing
  fonts <- liftIO $ loadFonts [(pxFontPath, 8), (ptsFontPath, 8)]
  newEntity $ Fonts $ createResourceMap fonts
  newEntity (Position $ V2 20 20, UIText "Test")
  newEntity $ SelectedConstruction Nothing
  return ()
