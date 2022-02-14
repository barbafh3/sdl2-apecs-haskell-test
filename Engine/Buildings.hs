{-# LANGUAGE TypeApplications           #-}

module Engine.Buildings (
  updateBuildings,
  emptyStorageList,
  addToStorage,
  removeFromStorage,
  totalUsage,
  resourceCount,
  requestHaulers,
  spawnHouse,
  spawnStorage
) where

import Engine.Components
import Engine.DataTypes
import Apecs
import Engine.Constants (haulerCapacity, tileSize, defaultRectSize, defaultRectSizeV2)
import Engine.Utils (truncate', gget, checkResourceInStorage)
import Linear (V2(..))
import Engine.World (System', World)
import Foreign.C (CInt)
import Control.Monad (when)

spawnHouse :: V2 Float -> StructureState -> System' Entity
spawnHouse pos state = do
  newEntity (
      Building state,
      EntityName "House",
      HaulRequest ("Wood", 60) 0,
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      StorageSpace [],
      BoundingBox pos (V2 8 8),
      InteractionBox pos defaultRectSizeV2,
      Position pos)

spawnStorage :: V2 Float -> [StorageItem] -> StructureState -> System' ()
spawnStorage pos storage state = do
  newEntity (
      Building state,
      EntityName "Storage",
      Sprite (V2 (6 * tileSize) (4 * tileSize)) defaultRectSize 1,
      BoundingBox pos (V2 8 8),
      InteractionBox pos defaultRectSizeV2,
      StorageSpace storage,
      Position pos)
  return ()

updateBuildings :: Float -> System'()
updateBuildings dT = do
  runBuildingTask
  followMouseCursor

emptyStorageList :: StorageList
emptyStorageList = [("Wood", 0)]

addToStorage :: StorageItem -> StorageList -> StorageList
addToStorage item [] = [item]
addToStorage item (pair : list)
    | fst pair == fst item = (fst pair, snd pair + snd item) : addToStorage item list
    | otherwise = addToStorage item list

removeFromStorage :: Maybe StorageItem -> StorageList -> StorageList
removeFromStorage Nothing [] = []
removeFromStorage (Just item) [] = [item]
removeFromStorage (Just item) [pair]
    | fst pair == fst item = if snd pair - snd item <= 0
                               then [(fst pair, 0)]
                                 else [(fst pair, snd pair - snd item)]
    | otherwise = [pair]
removeFromStorage (Just item) (pair : list)
    | fst pair == fst item = if snd pair - snd item <= 0
                               then (fst pair, 0) : removeFromStorage (Just item) list
                                 else (fst pair, snd pair - snd item) : removeFromStorage (Just item) list
    | otherwise = removeFromStorage (Just item) list
removeFromStorage Nothing (pair : list) = pair : list

totalUsage :: StorageList -> Int
totalUsage [] = 0
totalUsage [pair] = snd pair
totalUsage (pair : list) = snd pair + totalUsage list

resourceCount :: String -> StorageList -> Int
resourceCount _ [] = 0
resourceCount resource [pair]
    | fst pair == resource = snd pair
    | otherwise = 0
resourceCount resource (pair : list)
    | fst pair == resource = snd pair + resourceCount resource list
    | otherwise = resourceCount resource list

followMouseCursor :: System' ()
followMouseCursor = do
  cmapM $ \(Building state, building) -> do
    case state of
      Placement -> do
        MousePosition mPos@(V2 mx my) <- gget @MousePosition
        (InteractionBox ibPos ibSize, BoundingBox bbPos bbSize, Sprite _ (V2 sw sh) _) <- get building
        let pos = V2 (mx - (fromIntegral sw / 2)) (my - (fromIntegral sh / 2))
        set building (Position pos, InteractionBox pos ibSize, BoundingBox pos bbSize)
      _ -> return ()


runBuildingTask :: System' ()
runBuildingTask = cmapM_ $
  \(Building state, HaulRequest (resource, requiredamount) amount, building) -> do
    case state of
      Construction -> do
        let haulerCount = round $ (fromIntegral requiredamount :: Float) / haulerCapacity
        let (Entity ety) = building
        -- cfoldM_ (requestHaulers (HaulTask (resource, ceiling haulerCapacity) 4 ety) building) haulerCount
        cfoldM_ (findStorageAndRequestHaulers building haulerCount resource) 1
      _ -> return ()

requestHaulers :: HaulTask -> Entity -> Int -> (Villager, Entity) -> System' Int
requestHaulers haul building remainingSpots (Villager state, villager) =
  if remainingSpots == 0
    then pure 0
    else case state of
      Idle -> do
        set villager haul
        (HaulRequest (resource, requiredAmount) amount) <- get building
        let new_amount = round $ fromIntegral amount + haulerCapacity
        set building $
          if new_amount >= requiredAmount then
            Right $ Not @HaulRequest
          else
            Left $ HaulRequest (resource,  requiredAmount) new_amount
        pure (remainingSpots - 1)
      _ -> pure remainingSpots


findStorageAndRequestHaulers :: Entity -> Int -> String -> Int -> (StorageSpace, Entity) -> System' Int
findStorageAndRequestHaulers building@(Entity ety) haulerCount resource count (StorageSpace s, Entity st) = do
  liftIO $ print $ "Building: " ++ show (checkResourceInStorage s (resource, round haulerCapacity))
  liftIO $ print $ "Count: " ++ show count
  if count == 1 && checkResourceInStorage s (resource, round haulerCapacity)
    then do
        liftIO $ print "Requesting haulers..."
        cfoldM_ (requestHaulers (HaulTask (resource, ceiling haulerCapacity) st ety) building) haulerCount
        pure 0
      else pure count

