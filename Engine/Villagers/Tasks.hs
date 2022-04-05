{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Engine.Villagers.Tasks
  ( checkForHaulTask,
    checkEmptyBackpack,
    checkFilledBackpack,
    checkCarryDestination,
    checkPickupDestination,
    reachedDeliveryDestination,
    reachedPickupDestination,
    resetRequestHaulers,
  )
where

import Apecs
  ( Entity (Entity),
    Proxy (Proxy),
    cmap,
    cmapM,
    cmapM_,
    exists,
    get,
    liftIO,
    set,
  )
import Control.Monad (unless, when)
import Engine.Buildings.Storage (addToStorage, checkResourceInStorage, removeFromStorage)
import Engine.Collisions (areBoxesColliding)
import Engine.Components
  ( Backpack (Backpack),
    Building (Building),
    ConstructionStorage (..),
    HaulRequest (HaulRequest),
    HaulTask (..),
    IdleMovement (IdleMovement),
    Position (Position),
    StorageSpace (..),
    TargetPosition (TargetPosition),
    Villager (Villager),
  )
import Engine.DataTypes (EntityState (..))
import Engine.Utils (removeFromList)
import Engine.World (System')
import Linear (V2 (V2))

checkForHaulTask :: System' ()
checkForHaulTask = cmap $ \(Villager state, HaulTask {}) ->
  case state of
    Idle -> Villager Loading
    _ -> Villager state

checkEmptyBackpack :: System' ()
checkEmptyBackpack = cmapM_ $
  \(Villager state, HaulTask {}, Backpack mItem, villager) -> case state of
    Carrying -> case mItem of
      Just item -> return ()
      Nothing -> set villager (Villager Idle, Nothing :: (Maybe HaulTask))
    _ -> return ()

checkFilledBackpack :: System' ()
checkFilledBackpack = cmap $
  \(Villager state, HaulTask {}, Backpack mItem) -> case state of
    Loading -> case mItem of
      Just item -> Villager Carrying
      Nothing -> Villager state
    _ -> Villager state

checkCarryDestination :: System' ()
checkCarryDestination = cmapM_ $
  \(Villager state, HaulTask item orig dest, villager) ->
    when (state == Carrying) $ do
      (Building _, Position pos) <- get (Entity dest)
      (TargetPosition target) <- get villager
      unless (pos == target) do
        set villager (Villager state, TargetPosition pos)

checkPickupDestination :: System' ()
checkPickupDestination = cmapM $
  \(Villager state, HaulTask item orig dest, TargetPosition target) ->
    if state == Loading
      then
        get (Entity orig) >>= \case
          Just (Building _, StorageSpace s, Position bPos) -> do
            if checkResourceInStorage s item
              then return (Villager state, TargetPosition bPos)
              else return (Villager Idle, TargetPosition target)
          Nothing -> return (Villager Idle, TargetPosition target)
      else return (Villager state, TargetPosition target)

reachedDeliveryDestination :: System' ()
reachedDeliveryDestination = cmapM_ $
  \(Villager state, HaulTask item orig dest, villager) -> do
    when (state == Carrying) $ do
      buildingBox <- get (Entity dest)
      (Villager state, villagerBox, IdleMovement radius baseT t, Backpack bMItem) <- get villager
      when (areBoxesColliding buildingBox villagerBox) $ do
        case bMItem of
          Just bItem -> do
            hasStorage <- exists (Entity dest) (Proxy @StorageSpace)
            hasConstructionStorage <- exists (Entity dest) (Proxy @ConstructionStorage)
            if hasStorage
              then do
                StorageSpace storage <- get (Entity dest)
                set (Entity dest) $ StorageSpace $ addToStorage bItem storage
                set villager (Villager Idle, Backpack Nothing, IdleMovement radius baseT 0.0, Nothing :: (Maybe HaulTask))
              else
                if hasConstructionStorage
                  then do
                    (ConstructionStorage storage, HaulRequest st haulerList) <- get (Entity dest)
                    liftIO $ print $ show storage
                    let newHaulerList = removeFromList villager haulerList
                    set (Entity dest) (ConstructionStorage $ addToStorage bItem storage, HaulRequest st newHaulerList)
                    set villager (Villager Idle, Backpack Nothing, IdleMovement radius baseT 0.0, Nothing :: (Maybe HaulTask))
                  else set villager (Villager Idle, Backpack bMItem, IdleMovement radius baseT t, HaulTask item orig orig)
          Nothing -> return ()

reachedPickupDestination :: System' ()
reachedPickupDestination = cmapM_ $
  \(Villager state, HaulTask item orig dest, villager) ->
    when (state == Loading) $ do
      (buildingBox, StorageSpace storage) <- get (Entity orig)
      (Villager state, villagerBox, Backpack _) <- get villager
      when (areBoxesColliding buildingBox villagerBox) $ do
        let newStorage = removeFromStorage (Just item) storage
        unless (storage == newStorage) do
          set (Entity orig) $ StorageSpace newStorage
          set villager $ Backpack $ Just item

resetHauler :: Entity -> System' ()
resetHauler hauler = do
  (Backpack mItem, HaulTask item orig dest) <- get hauler
  case mItem of
    Just item -> set hauler $ HaulTask item orig orig
    Nothing -> return ()

resetRequestHaulers :: [Entity] -> System' ()
resetRequestHaulers [] = return ()
resetRequestHaulers [hauler] = resetHauler hauler
resetRequestHaulers (hauler : list) = do
  resetHauler hauler
  resetRequestHaulers list
