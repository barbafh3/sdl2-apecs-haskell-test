{-# LANGUAGE TypeApplications           #-}

module Engine.Buildings.Tasks (
    runBuildingTask,
    requestHaulers
    ) where
import Engine.Components
import Engine.World (System')
import Apecs
import Engine.DataTypes (StructureState(..), EntityState(..))
import Engine.Constants
import Engine.Buildings.Storage (checkResourceInStorage)

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
  if count == 1 && checkResourceInStorage s (resource, round haulerCapacity)
    then do
        cfoldM_ (requestHaulers (HaulTask (resource, ceiling haulerCapacity) st ety) building) haulerCount
        pure 0
      else pure count
