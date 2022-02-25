{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Engine.Buildings.Tasks (
    runBuildingTask,
    requestHaulers
    ) where
import Engine.Components
import Engine.World (System')
import Apecs
import Engine.DataTypes (StructureState(..), EntityState(..), StorageItem)
import Engine.Constants
import Engine.Buildings.Storage (checkResourceInStorage)
import Data.Foldable (for_, traverse_)
import Control.Monad (when)

runBuildingTask :: System' ()
runBuildingTask = do 
  cmapM_ $
    \(Building state, HaulRequest taskItemList, ConstructionStorage itemList, building) -> do
      case state of
        Construction -> do
          if taskItemList == itemList
             then do
               destroy building (Proxy @HaulRequest)
               destroy building (Proxy @ConstructionStorage)
               set building $ Building Enabled
             else traverse_ (\item -> cfoldM_ (findStorageAndRequestHaulers building item) 1) taskItemList
        _ -> return ()

requestHaulers :: StorageItem -> HaulTask -> Entity -> Int -> (Villager, Entity) -> System' Int
requestHaulers (resource, requiredAmount) haul building remainingSpots (Villager state, villager) =
  if remainingSpots == 0
    then pure 0
    else case state of
      Idle -> do
        set villager haul
        pure (remainingSpots - 1)
      _ -> pure remainingSpots

findStorageAndRequestHaulers :: Entity -> StorageItem -> Int -> (StorageSpace, Entity) -> System' Int
findStorageAndRequestHaulers building@(Entity ety) item@(resource, requiredAmount) count (StorageSpace s, Entity st) = do
  let haulerCount = ceiling $ (fromIntegral requiredAmount :: Float) / haulerCapacity
  if count == 1 && checkResourceInStorage s (resource, round haulerCapacity)
    then do
        cfoldM_ (requestHaulers item (HaulTask (resource, ceiling haulerCapacity) st ety) building) haulerCount
        pure 0
      else pure count
