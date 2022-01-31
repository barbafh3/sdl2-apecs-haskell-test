{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Rendering (draw) where

import Apecs
import qualified SDL
import Linear (V2(..), V4(..))
import Foreign.C.Types
import Engine.World
import Engine.Components
import Colors
import Engine.Utils (gget, getRelativeBoxPosition)
import SDL (Texture)
import SDL.Raw (Color(Color))
import qualified Data.HashMap.Strict as HM
import SDL.Font (solid, Color, size, Font)
import Data.Text (Text, pack)
import Control.Arrow ((***))
import SDL.Video (Rectangle(Rectangle))
import Engine.Constants (pxFontPath, ptsFontPath)
import qualified SDL.Raw.Primitive as SDL.Primitive
import qualified SDL.Primitive
import Debug.Trace (trace)
import Engine.DataTypes (ClickState(..))

drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> System' ()
drawTexture = SDL.copy

draw :: SDL.Renderer -> SDL.Texture -> Int -> System' ()
draw renderer tileset fps = do
  SDL.rendererDrawColor renderer SDL.$= V4 116 210 102 255
  SDL.clear renderer

  drawUI renderer tileset
  drawSprites renderer tileset

  SDL.present renderer

drawSprites ::SDL.Renderer -> Texture -> System' ()
drawSprites renderer tileset =
  cmapM_ $ \(Position pos, Sprite coord size scale, _ :: Not Button) -> do
          drawTexture renderer tileset (Just $ SDL.Rectangle (SDL.P coord) size) (Just $ SDL.Rectangle (SDL.P (round <$> pos)) ((* round scale) <$> size))

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

blackP :: SDL.Primitive.Color
blackP = V4 0 0 0 255

blackPA :: SDL.Primitive.Color
blackPA = V4 0 0 0 178

whitePA :: SDL.Primitive.Color
whitePA = V4 255 255 255 100

blackPA2 :: SDL.Primitive.Color
blackPA2 = V4 0 0 0 100

brownA :: SDL.Primitive.Color
brownA = V4 100 60 2 200

drawUI ::SDL.Renderer -> SDL.Texture -> System' ()
drawUI renderer tileset = do
  Fonts fonts  <- gget @Fonts
  let mFont = HM.lookup pxFontPath fonts
  let mFont2 = HM.lookup ptsFontPath fonts
  drawUIBoxes renderer tileset
  cmapM_ $ \(Position pos, UIText t) -> do
          case mFont2 of
            Just font -> do
              let text = pack t
              drawText renderer font pos text
            Nothing -> return ()
  buildingCount <- cfoldM countBuildings 0
  cfoldM_ (drawBuildingInfo renderer mFont (V2 20 40) buildingCount) 0
  drawParticles renderer

drawUIBoxes :: SDL.Renderer -> SDL.Texture -> System' ()
drawUIBoxes renderer tileset = cmapM_ $
  \(Button cState hover toggled, InterfaceBox bSize@(V2 bw bh), Position pos@(V2 x y), Sprite coord size@(V2 sw sh) scale) -> do
      let color = if hover then blackPA2 else blackPA
      let  color' = case cState of 
                      Clicked -> blackP 
                      _ -> color
      drawBox renderer (round <$> pos) (round <$> bSize) ((* round scale) <$> size) color'
      let centerPos = V2 (x + ((bw / 2) - (fromIntegral sw * scale / 2))) (y + ((bh / 2) - (fromIntegral sh * scale / 2)))
      drawTexture renderer tileset (Just $ SDL.Rectangle (SDL.P coord) size) (Just $ SDL.Rectangle (SDL.P (round <$> centerPos)) ((* round scale) <$> size))

drawBox :: SDL.Renderer -> V2 CInt -> V2 CInt -> V2 CInt -> SDL.Primitive.Color -> System' ()
drawBox renderer oPos@(V2 x y) bSize@(V2 w h) size@(V2 sw sh) color = do
  let oPos' = V2 (x + w) (y + h)
  let iPos@(V2 px py) =  V2 (x + ((w `div` 2) - (fromIntegral sw `div` 2))) (y + ((h `div` 2) - (fromIntegral sh `div` 2)))
  SDL.Primitive.fillRectangle renderer oPos oPos' color
  SDL.Primitive.fillRectangle renderer iPos (V2 (px + sw) (py + sh)) whitePA

-- drawBox :: SDL.Renderer -> V2 CInt -> V2 CInt -> V2 CInt -> SDL.Primitive.Color -> System' ()
-- drawBox renderer pos@(V2 x y) bSize@(V2 w h) size@(V2 sw sh) color = do
--   let (pos1, pos2) = getRelativeBoxPosition pos bSize size
--   SDL.Primitive.fillRectangle renderer pos1 pos2 color
--   SDL.Primitive.fillRectangle renderer pos (V2 (x + sw) (y + sh)) whitePA

drawParticles :: SDL.Renderer -> System' ()
drawParticles renderer = cmapM_ $
  \(Particle t, Position pos) -> do
    SDL.Primitive.fillCircle renderer (round <$> pos) (round t) brownA
    return ()

drawBuildingInfo :: SDL.Renderer ->
  Maybe Font ->
  V2 Float ->
  Int ->
  Int ->
  (Building, EntityName, StorageSpace, Maybe HaulRequest) ->
  System' Int
drawBuildingInfo renderer mFont (V2 x y) baseCount count (Building _, EntityName name, StorageSpace space, Just request) = do
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
drawBuildingInfo renderer mFont (V2 x y) baseCount count (Building _, EntityName name, StorageSpace space, Nothing) = do
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
