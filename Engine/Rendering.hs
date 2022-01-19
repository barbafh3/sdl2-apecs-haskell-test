{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine.Rendering (draw) where

import Apecs
import qualified SDL
import Linear (V2(..), V4(..))
import Foreign.C.Types
import Engine.World
import Engine.Components
import Colors
import Engine.Utils (gget)
import SDL (Texture)
import SDL.Raw (Color(Color))
import qualified Data.HashMap.Strict as HM
import SDL.Font (solid, Color, size, Font)
import Data.Text (Text, pack)
import Control.Arrow ((***))
import SDL.Video (Rectangle(Rectangle))

drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> System' ()
drawTexture = SDL.copy

draw :: SDL.Renderer -> SDL.Texture -> Int -> System' ()
draw renderer tileset fps = do
  SDL.rendererDrawColor renderer SDL.$= V4 116 210 102 255
  SDL.clear renderer

  drawSprites renderer tileset
  drawUI renderer

  SDL.present renderer

drawSprites ::SDL.Renderer -> Texture -> System' ()
drawSprites renderer tileset =
  cmapM_ $ \(Position pos, Sprite coord size) -> do
          drawTexture renderer tileset (Just $ SDL.Rectangle (SDL.P coord) size) (Just $ SDL.Rectangle (SDL.P (round <$> pos)) size)

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

drawUI ::SDL.Renderer -> System' ()
drawUI renderer = do
  Fonts fonts  <- gget @Fonts
  let mFont = HM.lookup "Assets/prstartk.ttf" fonts
  cmapM_ $ \(Position pos, UIText t) -> do
          case mFont of
            Just font -> do
              let text = pack t
              drawText renderer font pos text
            Nothing -> return ()
  buildingCount <- cfoldM countBuildings 0
  cfoldM_ (drawBuildingInfo renderer mFont (V2 20 40) buildingCount) 0

drawBuildingInfo :: SDL.Renderer -> Maybe Font -> V2 Float -> Int -> Int -> (Building, EntityName, StorageSpace, Maybe HaulRequest) -> System' Int
drawBuildingInfo renderer mFont (V2 x y) baseCount count (Building, EntityName name, StorageSpace space, Just request) = do
  if count < baseCount 
    then
       case mFont of
         Just font -> do
           let text = pack $ name ++ " - Storage: " ++ show space ++ " - Request: " ++ show request
           let newY = if count == 0 then y else y + (20 * fromIntegral count)
           drawText renderer font (V2 x newY) text
           pure $ count + 1
         Nothing -> pure count
      else pure count    
drawBuildingInfo renderer mFont (V2 x y) baseCount count (Building, EntityName name, StorageSpace space, Nothing) = do
  if count < baseCount 
    then
       case mFont of
         Just font -> do
           let text = pack $ name ++ " - Storage: " ++ show space
           let newY = if count == 0 then y else y + (20 * fromIntegral count)
           drawText renderer font (V2 x newY) text
           pure $ count + 1
         Nothing -> pure count
      else pure count    

countBuildings :: Int -> (Building, EntityName, StorageSpace) -> System' Int
countBuildings count (_, _, _) = pure $ count + 1

drawText :: SDL.Renderer -> Font -> V2 Float -> Text -> System' ()
drawText renderer font pos text = do
  textSurf <- solid font black text
  textTex <- SDL.createTextureFromSurface renderer textSurf
  fontSize <- SDL.Font.size font text
  let (w, h) = (fromIntegral *** fromIntegral) fontSize
  SDL.copy renderer textTex Nothing (Just (Rectangle (SDL.P (round <$> pos)) (V2 w h)))
  SDL.freeSurface textSurf

drawPlayer :: SDL.Renderer -> System' ()
drawPlayer renderer = return ()
  -- cmapM_ (\(Player, Sprite sp sz, Position p) -> P.fillCircle renderer p r $ V4 113 113 255 255)


drawFilledCircles :: SDL.Renderer -> System' ()
drawFilledCircles renderer = return ()
  -- cmapM_ (\(Circle r, Position p) -> P.fillCircle renderer p r $ V4 113 113 255 255)

drawLines :: SDL.Renderer -> System' ()
drawLines renderer = return ()
  -- cmapM_ (\(Line p1 p2) -> P.line renderer p1 p2 $ V4 255 255 255 255)
