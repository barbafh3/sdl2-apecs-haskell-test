module Engine.Input (handleInputPayload) where

import Apecs
import Engine.Collisions (isInsideInteractionBox, isInsideBoundingBox, isInsideInteractionBoxI)
import Engine.Components
import Engine.Constants (defaultRectSizeV2)
import Control.Monad (when)
import qualified Control.Monad
import Engine.DataTypes (DrawLevels (..), EntityState (Idle), ClickState (..))
import Linear (V2 (V2))
import Engine.Particles (spawnParticles)
import Engine.Utils (gget, getRelativeBoxPosition, (<#>))
import Engine.Buildings (requestHaulers)
import Engine.World (System')
import SDL (
  EventPayload (KeyboardEvent, MouseMotionEvent),
  MouseButtonEventData (mouseButtonEventMotion),
  KeyboardEventData,
  MouseMotionEventData (mouseMotionEventPos),
  InputMotion (Pressed, Released),
  getAbsoluteMouseLocation, Point (P))
import SDL.Event
    ( EventPayload(MouseButtonEvent, MouseMotionEvent),
      MouseButtonEventData(mouseButtonEventButton),
      MouseButton(ButtonLeft),
      MouseButtonEventData(mouseButtonEventPos) )

handleInputPayload :: [EventPayload] -> System' ()
handleInputPayload [] = return ()
handleInputPayload [MouseButtonEvent ev] = handleMouseEvent ev
handleInputPayload [MouseMotionEvent ev] = handleMouseMotionEvent ev
handleInputPayload [KeyboardEvent ev] = handleKeyboardEvent ev
handleInputPayload [_] = return ()
handleInputPayload (MouseButtonEvent ev : list) = do
  handleMouseEvent ev
  handleInputPayload list
handleInputPayload (MouseMotionEvent ev : list) = do
  handleMouseMotionEvent ev
  handleInputPayload list
handleInputPayload (KeyboardEvent ev : list) = do
  handleKeyboardEvent ev
  handleInputPayload list
handleInputPayload (_ : list) = handleInputPayload list

handleMouseMotionEvent :: MouseMotionEventData -> System' ()
handleMouseMotionEvent ev = do
  let (P (V2 mx my)) = mouseMotionEventPos ev
  cmapM_ $
    \(Button clicked _ toggled, InterfaceBox bSize, Position pos, Sprite _ size _, button) -> do
          let intMPos = V2 (fromIntegral mx) (fromIntegral my)
          let box = InteractionBox pos bSize
          if isInsideInteractionBoxI intMPos box
             then set button (Button clicked True toggled)
               else set button (Button clicked False toggled)

handleMouseEvent :: MouseButtonEventData -> System' ()
handleMouseEvent ev =
  case mouseButtonEventMotion ev of
    Pressed -> case mouseButtonEventButton ev of
                 ButtonLeft -> do
                   (SDL.P (V2 mx my)) <- getAbsoluteMouseLocation
                   cmapM_ $ \(Button cState hover toggled, InterfaceBox bSize, Position pos, Sprite _ size _, button) -> do
                     case cState of
                       Clicked -> set button (Button ClickHeld hover toggled)
                       NotClicked -> do
                         let intMPos = V2 (fromIntegral mx) (fromIntegral my)
                         let relativePos = getRelativeBoxPosition (round <$> pos) (round <$> bSize) size
                         let box = InteractionBox (fromIntegral <#> fst relativePos) bSize
                         when (isInsideInteractionBoxI intMPos box) $ set button (Button Clicked hover toggled)
                       _ -> return ()
                 _ -> return()
    Released -> cmapM_ $ 
      \(Button cState hover toggled, button) -> 
        case cState of
          ClickReleased -> set button $ Button NotClicked hover toggled
          _ -> set button $ Button ClickReleased hover toggled



handleKeyboardEvent :: KeyboardEventData -> System' ()
handleKeyboardEvent ev = return ()
-- handleEvent :: Event -> System' ()
-- handleEvent (EventMotion (x, y)) = set global $ MousePosition (V2 x y)

-- handleEvent (EventKey (SpecialKey KeyF10) Down _ _) = do
--   drawLevel <- gget @DrawLevel
--   case drawLevel of
--     DrawLevel Particles -> set global $ DrawLevel Default
--     _ -> set global $ DrawLevel Particles

-- handleEvent (EventKey (SpecialKey KeyF11) Down _ _) = do
--   drawLevel <- gget @DrawLevel
--   case drawLevel of
--     DrawLevel Collision -> set global $ DrawLevel Default
--     _ -> set global $ DrawLevel Collision

-- handleEvent (EventKey (SpecialKey KeyF9) Down _ _) = do
--   drawLevel <- gget @DrawLevel
--   case drawLevel of
--     DrawLevel All -> set global $ DrawLevel Default
--     _ -> set global $ DrawLevel All

-- handleEvent (EventKey (SpecialKey KeyF8) Down _ _) = do
--   drawLevel <- gget @DrawLevel
--   case drawLevel of
--     DrawLevel Debug -> set global $ DrawLevel Default
--     _ -> set global $ DrawLevel Debug

-- handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) = do
--   cmapM_ $ 
--     \(Position p, InteractionBox pos size, StorageSpace storage, entity) -> do
--       (InfoPanel ety) <- gget @InfoPanel
--       let newPos = pos - (defaultRectSizeV2 / 2)
--       if isInsideInteractionBox (V2 x y) (InteractionBox newPos size)
--         then do
--           set global $ InfoPanel entity
--           spawnParticles 5
--         else Control.Monad.when (ety == entity) $ set global $ InfoPanel Nothing
--   cmapM $ \(Button clicked, Position pos, InteractionBox _ size) -> do 
--     let newPos = pos - defaultRectSizeV2
--     if isInsideInteractionBox (V2 x y) (InteractionBox newPos size) 
--       then return $ Button True
--         else return $ Button False

-- handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) = cmap $ \(Button clicked) -> Button False

-- handleEvent _ = return ()

changeIdlePoint :: Int -> Int -> System' ()
changeIdlePoint ety1 ety2 = cmapM_ $
  \(Building _, Position pos, Entity e1) ->
    when (e1 == ety1) $ cmap $
      \(Villager _, IdlePoint ip, Entity e2) ->
        if e2 == ety2 then IdlePoint pos
        else IdlePoint ip

