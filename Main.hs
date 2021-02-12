{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Control.Monad
import Linear (V2 (..))
import qualified SDL
import qualified SDL.Image

newtype Position = Position (V2 Double) deriving (Show)

instance Component Position where type Storage Position = Map Position

data Player = Player deriving (Show)

instance Component Player where type Storage Player = Unique Player

newtype Name = Name String deriving (Show)

instance Component Name where type Storage Name = Map Name

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Position, ''Player, ''Name]

type System' a = System World a

main :: IO ()
main = do
  world <- initWorld
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "App" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.RendererConfig {SDL.rendererType = SDL.AcceleratedRenderer, SDL.rendererTargetTexture = False}
  SDL.showWindow window

  let loop prevTicks secondTick fpsAcc prevFps = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            delta = ticks - prevTicks
            calcFps = secondTick + delta > 1000
            newFps = if calcFps then fpsAcc + 1 else prevFps
            newFpsAcc = if calcFps then 1 else fpsAcc + 1
            newNextTicks = if calcFps then mod (secondTick + delta) 1000 else secondTick + delta

        runSystem initializeEntities world
        runSystem (gameLoop $ fromIntegral delta) world
        runSystem (draw renderer) world

        unless quit $ loop ticks newNextTicks newFpsAcc newFps

  loop 0 0 0 0

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.quit
  putStrLn "Goodbye!"

--   exitSuccess

gameLoop :: Double -> System' ()
gameLoop delta = do
  moveCharacters delta
  printPositions

draw :: SDL.Renderer -> System' ()
draw renderer = do
  texture <- SDL.Image.loadTexture renderer "tileset.png"
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer

initializeEntities :: System' Entity
initializeEntities = do
  liftIO $ putStrLn "Let's do some IO just to prove we can!"
  newEntity (Player, Name "Buba", Position $ V2 0.0 0.0)
  newEntity (Name "Jose", Position $ V2 0.0 0.0)

moveCharacters :: Double -> System' ()
moveCharacters dT =
  cmap (\(Position (V2 x y)) -> Position $ V2 (x + dT) (y + dT))

printPositions :: System' ()
printPositions = do
  cmapM_ (\(Position (V2 x y)) -> liftIO $ putStrLn (show x ++ " " ++ show y))