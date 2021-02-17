module GameRendering (draw) where

import Apecs
import qualified SDL
import Linear (V2(..))
import Foreign.C.Types

import GameSystem
import GameComponents

draw :: SDL.Renderer -> SDL.Texture -> System' ()
draw renderer tileset = do
  SDL.clear renderer
  cmapM_ (\(Position p, (Sprite sc sr)) -> do
          let srcRect = SDL.Rectangle (SDL.P sc) sr
          let dstRect = SDL.Rectangle (SDL.P p) sr
          let src = Just (srcRect)
          let dst = Just (dstRect)
          SDL.copy renderer tileset src dst
          )
  SDL.present renderer
