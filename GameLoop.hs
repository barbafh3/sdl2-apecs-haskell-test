{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameLoop (gameLoop, moveCharacters, printPositions, filterPayload, handleEvent) where

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
handlePlayerInput [event] delta = filterPayload event
handlePlayerInput (event:events) delta = do
  filterPayload event
  handlePlayerInput events delta

filterPayload :: SDL.EventPayload -> System' ()
filterPayload (SDL.KeyboardEvent k) = handleEvent k
filterPayload _ = return ()
  
handleEvent :: SDL.KeyboardEventData -> System' ()
handleEvent (SDL.KeyboardEventData _ _ _ keysym) = do
  let key = SDL.keysymKeycode keysym
  cmap (\(Player, Position (V2 x y), Speed speed) -> do
    Position $ filterMovementKeys key speed $ V2 x y)

filterMovementKeys :: SDL.Keycode -> CInt -> V2 CInt -> V2 CInt
filterMovementKeys keycode speed (V2 x y) =
  case keycode of
    SDL.KeycodeW -> V2 x (y - speed)
    SDL.KeycodeS -> V2 x (y + speed)
    SDL.KeycodeA -> V2 (x - speed) y
    SDL.KeycodeD -> V2 (x + speed) y
    _ -> V2 x y
