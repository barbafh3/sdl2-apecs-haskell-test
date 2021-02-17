{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameLoop (gameLoop, moveCharacters, printPositions) where

import Apecs
import qualified SDL
import Linear(V2(..))
import Foreign.C.Types

import GameSystem
import GameComponents

gameLoop :: Double -> [SDL.EventPayload] -> System' ()
gameLoop delta payload = do
  handlePlayerInput payload delta

moveCharacters :: System' ()
moveCharacters =
  cmap (\(Position (V2 x y), Speed s) -> Position $ V2 (x + s) (y + s))

printPositions :: System' ()
printPositions = do
  cmapM_ (\(Position (V2 x y)) -> liftIO $ putStrLn (show x ++ " " ++ show y))


handlePlayerInput :: [SDL.EventPayload] -> Double -> System' ()
handlePlayerInput [] _ = return ()
handlePlayerInput [event] delta = handleEvent event
handlePlayerInput (x:xs) delta = do
  handleEvent x
  handlePlayerInput xs delta
  

handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.KeyboardEventData _ SDL.Pressed False keysym) = 
  case SDL.keysymKeyCode keysym of
    SDL.KeycodeW -> do cmap (\(Player, Position (V2 x y), Speed s) -> Position $ V2 x (y - s)
    SDL.KeycodeS -> do cmap (\(Player, Position (V2 x y), Speed s) -> Position $ V2 x (y + s)
    SDL.KeycodeA -> do cmap (\(Player, Position (V2 x y), Speed s) -> Position $ V2 x (x - s)
    SDL.KeycodeD -> do cmap (\(Player, Position (V2 x y), Speed s) -> Position $ V2 x (x + s)
    _ -> return ()
handleEvent _ = return ()
  
  -- cmap (\(Player, Position (V2 x y), Speed s) -> Position $ V2 (x + s) (y + s))
