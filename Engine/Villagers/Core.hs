{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Engine.Villagers.Core (
  idleTick,
  checkIdleTimer,
  moveToTarget,
  updateVillagerCollisions,
  updateVillagers,
) where

import Engine.Components
import System.Random
import Linear (V2(V2))
import Apecs
import Engine.Utils (normalizeVectorF, vectorLengthF)
import Engine.DataTypes(EntityState(..))
import Engine.Buildings.Storage (addToStorage, removeFromStorage, checkResourceInStorage)
import Engine.Collisions (areBoxesColliding)
import Control.Monad (when, unless)
import Engine.World (System')

updateVillagers :: Float ->  System' ()
updateVillagers dT = do
  runIdleState dT
  runCarryingState dT
  runLoadingState dT
  moveToTarget dT
  updateVillagerCollisions dT

runIdleState :: Float -> System' ()
runIdleState dT = do
  checkForHaulTask
  idleTick dT
  checkIdleTimer dT

runCarryingState :: Float -> System' ()
runCarryingState dT = do
  checkCarryDestination
  checkEmptyBackpack
  reachedDeliveryDestination

runLoadingState :: Float -> System' ()
runLoadingState dT = do
  checkPickupDestination
  checkFilledBackpack
  reachedPickupDestination

moveToTarget :: Float -> System' ()
moveToTarget dT = cmap $
    \(Villager state, Position pos, TargetPosition tPos, Velocity (V2 vx vy), Entity e) ->
        if vectorLengthF (tPos - pos) > 2.0
           then Position $ pos + ((* dT) . (* vx) <$> normalizeVectorF (tPos - pos))
              else Position pos

idleTick :: Float -> System' ()
idleTick dT = cmap $ \(Villager state, IdleMovement radius baseT idleT) ->
    case state of
        Idle -> if idleT > 0
                     then IdleMovement radius baseT (idleT - dT)
                       else IdleMovement radius baseT idleT
        _ -> IdleMovement radius baseT idleT

getNewTarget :: StdGen -> V2 Float -> Float -> (StdGen, V2 Float)
getNewTarget rng (V2 px py) radius = (rng'', V2 nx ny)
    where
        (minX, minY) = (px - radius, py - radius)
        (maxX, maxY) = (px + radius, py + radius)
        (nx, rng') = uniformR (minX, maxX) rng
        (ny, rng'') = uniformR (minY, maxY) rng'

checkIdleTimer :: Float -> System' ()
checkIdleTimer dt = cmap checkTimer
  where
    checkTimer (Villager state, IdleMovement radius baseT idleT, IdlePoint ip, TargetPosition mPos, Rng rng) = case state of
      Idle -> if idleT <= 0.0
        then (IdleMovement radius baseT baseT, TargetPosition newTarget, Rng rng')
          else (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)
             where
               (rng', newTarget) = getNewTarget rng ip radius
      _ -> (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)

checkForHaulTask :: System' ()
checkForHaulTask = cmap $ \(Villager state, HaulTask {}) ->
  case state of
    Idle -> Villager Loading
    _ -> Villager state

updateVillagerCollisions :: Float -> System'()
updateVillagerCollisions _ =
  cmap $ \(Villager _, Position (V2 x y), BoundingBox (V2 rx ry) size) -> BoundingBox (V2 x y) size

checkEmptyBackpack :: System' ()
checkEmptyBackpack = cmapM_ $
  \(Villager state, HaulTask {}, Backpack mItem, villager) -> case state of
    Carrying -> case mItem of
                    Just item -> return ()
                    Nothing -> set villager (Villager Idle, Nothing :: (Maybe HaulTask))
    _ ->  return ()

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
      then get (Entity orig) >>= \case
          Just (Building _, StorageSpace s, Position bPos) -> do 
            if checkResourceInStorage s item
               then return (Villager state, TargetPosition bPos)
                 else return (Villager Idle, TargetPosition target)
          Nothing -> return (Villager Idle, TargetPosition target)
        else return (Villager state, TargetPosition target)

reachedDeliveryDestination :: System' ()
reachedDeliveryDestination = cmapM_ $
    \(Villager state, HaulTask item orig dest, villager) -> when (state == Carrying) $ do
      (buildingBox, StorageSpace storage) <- get (Entity dest)
      (Villager state, villagerBox, IdleMovement radius baseT _, Backpack bMItem) <- get villager
      when (areBoxesColliding buildingBox villagerBox) $ do
        case bMItem of
          Just bItem -> do
            set (Entity dest) $ StorageSpace $ addToStorage bItem storage
            set villager (Villager Idle, Backpack Nothing, IdleMovement radius baseT 0.0, Nothing :: (Maybe HaulTask))
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
