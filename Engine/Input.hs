{-# LANGUAGE TypeApplications #-}

module Engine.Input (handleInputPayload) where

import Apecs
  ( Entity (Entity),
    Proxy (Proxy),
    cmap,
    cmapM_,
    destroy,
    get,
    global,
    set,
  )
import Control.Monad (unless, when)
import qualified Control.Monad
import Data.Monoid (getAny)
import Data.Semigroup (Any (Any))
import Engine.Buildings.Tasks (requestHaulers)
import Engine.Collisions (areBoxesColliding, isInsideBoundingBox, isInsideInteractionBox, isInsideInteractionBoxI)
import Engine.Components
  ( Building (Building),
    Button (Button),
    IdlePoint (IdlePoint),
    InteractionBox (InteractionBox),
    InterfaceBox (InterfaceBox),
    MousePosition (..),
    PlacementHouse,
    Position (Position),
    SelectedConstruction (..),
    Sprite (Sprite),
    Villager (Villager),
  )
import Engine.Constants (defaultRectSizeV2)
import Engine.DataTypes (ClickState (..), DrawLevels (..), EntityState (Idle), StructureState (..))
import Engine.Particles (spawnParticles)
import Engine.Utils (cfoldMap, getRelativeBoxPosition, gget, (<#>))
import Engine.World (System')
import Linear (V2 (V2))
import SDL
  ( EventPayload (KeyboardEvent, MouseMotionEvent),
    InputMotion (Pressed, Released),
    KeyboardEventData,
    MouseButton (ButtonRight),
    MouseButtonEventData (mouseButtonEventMotion),
    MouseMotionEventData (mouseMotionEventPos),
    Point (P),
    getAbsoluteMouseLocation,
  )
import SDL.Event
  ( EventPayload (MouseButtonEvent, MouseMotionEvent),
    MouseButton (ButtonLeft),
    MouseButtonEventData (mouseButtonEventButton, mouseButtonEventPos),
  )

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
  let (P mPos) = mouseMotionEventPos ev
  cmapM_ $
    \(Button clicked _ toggled, InterfaceBox bSize, Position pos, Sprite _ size _, button) -> do
      let box = InteractionBox pos bSize
      if isInsideInteractionBoxI (fromIntegral <$> mPos) box
        then set button (Button clicked True toggled)
        else set button (Button clicked False toggled)

handleMouseEvent :: MouseButtonEventData -> System' ()
handleMouseEvent ev =
  case mouseButtonEventMotion ev of
    Pressed -> case mouseButtonEventButton ev of
      ButtonLeft -> handleLeftMousePress
      ButtonRight -> handdleRightMousePress
      _ -> return ()
    Released -> cmapM_ $
      \(Button cState hover toggled, button) ->
        case cState of
          ClickReleased -> set button $ Button NotClicked hover toggled
          _ -> set button $ Button ClickReleased hover toggled

handleLeftMousePress = do
  MousePosition mPos <- gget @MousePosition
  SelectedConstruction msc <- gget @SelectedConstruction
  case msc of
    Just ety -> do
      bBox <- get ety
      isColliding <- fmap getAny $
        cfoldMap $
          \(Building _, box, building) -> do
            Any $ (ety /= building) && areBoxesColliding bBox box
      unless isColliding $ do
        set ety $ Building Construction
        set global $ SelectedConstruction Nothing
    Nothing -> return ()
  cmapM_ $ \(Button cState hover toggled, InterfaceBox bSize, Position pos, Sprite _ size _, button) -> do
    case cState of
      Clicked -> set button (Button ClickHeld hover toggled)
      NotClicked -> do
        let box = InteractionBox pos bSize
        when (isInsideInteractionBoxI mPos box) $ set button (Button Clicked hover toggled)
      _ -> return ()

handdleRightMousePress = do
  SelectedConstruction msc <- gget @SelectedConstruction
  case msc of
    Just ety -> do
      destroy ety (Proxy @PlacementHouse)
      set global $ SelectedConstruction Nothing
    Nothing -> return ()

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
    when (e1 == ety1) $
      cmap $
        \(Villager _, IdlePoint ip, Entity e2) ->
          if e2 == ety2
            then IdlePoint pos
            else IdlePoint ip
