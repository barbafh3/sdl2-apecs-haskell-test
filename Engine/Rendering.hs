{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Rendering (draw) where

import Foreign.C.Types ( CInt )
import Control.Arrow ((***))
import Data.Text (Text, pack)
import qualified Data.HashMap.Strict as HM
import Linear (V2(..), V4(..))
import Debug.Trace (trace)
import Apecs ( cfoldM, cfoldM_, cmapM_, Not, get )
import qualified SDL
import SDL (Texture, ($=))
import SDL.Raw (Color(Color))
import SDL.Font (solid, Color, size, Font)
import SDL.Video ( Rectangle(Rectangle), textureColorMod )
import qualified SDL.Primitive
import Engine.Utils (gget, getRelativeBoxPosition)
import Engine.Constants (pxFontPath, ptsFontPath)
import Engine.DataTypes (ClickState(..), StructureState (Placement, Enabled, Construction), GameTextures)
import Engine.Colors
    ( black, blackF, blackP, blackPA, blackPA2, brownA, whitePA, background, backgroundA, white, red )
import Engine.World ( System' )
import Engine.Components
    ( Building(..),
      Button(..),
      EntityName(..),
      Fonts(..),
      HaulRequest,
      InterfaceBox(InterfaceBox),
      Particle(Particle),
      Position(Position),
      Sprite(Sprite),
      StorageSpace(..),
      UIText(UIText), Villager (Villager), MousePosition (MousePosition) )
import qualified SDL.Raw
import qualified SDL.Video
import qualified SDL.Raw.Video
import Control.Monad.IO.Class (liftIO)
import Apecs.Core (Entity)
import Apecs.Core (Entity(Entity))
import Engine.Collisions (areBoxesColliding)
import Control.Monad (when)
-- import Data.StateVar (($=))

draw :: SDL.Renderer -> Texture -> Int -> System' ()
draw renderer tileset fps = do
  SDL.rendererDrawColor renderer SDL.$= backgroundA
  SDL.clear renderer

  drawBuildings renderer tileset
  drawVillagers renderer tileset
  drawUI renderer tileset

  SDL.present renderer

drawTexture :: SDL.Renderer -> Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> System' ()
drawTexture = SDL.copy

drawVillagers ::SDL.Renderer -> Texture -> System' ()
drawVillagers renderer tileset =
  cmapM_ $
    \(Position pos, Sprite coord size scale, Villager _) -> do
      textureColorMod tileset $= white
      drawTexture
        renderer
        tileset
        (Just $ SDL.Rectangle (SDL.P coord) size)
        (Just $ SDL.Rectangle (SDL.P (round <$> pos))
        ((* round scale) <$> size))

checkPlacementCollision :: Entity -> Texture -> System' ()
checkPlacementCollision ety@(Entity eEty) tileset = do
  (Position ePos, eBox) <- get ety
  cmapM_ $
    \(Building _, Position pos, box, Entity bEty) -> do
      when (areBoxesColliding eBox box && (eEty /= bEty)) $ textureColorMod tileset $= red

drawBuildings :: SDL.Renderer -> Texture -> System' ()
drawBuildings renderer tileset = do
  cmapM_ $
    \(Position pos, Sprite coord size scale, Building state, building) -> do
      case state of
        Placement -> do
          textureColorMod tileset $= background
          checkPlacementCollision building tileset
          drawTexture
            renderer
            tileset
            (Just $ SDL.Rectangle (SDL.P coord) size)
            (Just $ SDL.Rectangle (SDL.P (round <$> pos))
            ((* round scale) <$> size))
        Construction -> do
          textureColorMod tileset $= white
          drawTexture
            renderer
            tileset
            (Just $ SDL.Rectangle (SDL.P coord) size)
            (Just $ SDL.Rectangle (SDL.P (round <$> pos))
            ((* round scale) <$> size))
        Enabled -> do
          textureColorMod tileset $= white
          drawTexture
            renderer
            tileset
            (Just $ SDL.Rectangle (SDL.P coord) size)
            (Just $ SDL.Rectangle (SDL.P (round <$> pos))
            ((* round scale) <$> size))
        _ -> return ()

drawUI :: SDL.Renderer -> Texture -> System' ()
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

drawUIBoxes :: SDL.Renderer -> Texture -> System' ()
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
  textSurf <- solid font blackF text
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
