{-# LANGUAGE BlockArguments #-}

module Engine.Villagers.Core
  ( idleTick,
    checkIdleTimer,
    moveToTarget,
    updateVillagerCollisions,
    updateVillagers,
  )
where

import Apecs (Entity (Entity), cmap)
import Control.Monad (unless, when)
import Engine.Buildings.Storage (addToStorage, checkResourceInStorage, removeFromStorage)
import Engine.Collisions (areBoxesColliding)
import Engine.Components
  ( BoundingBox (BoundingBox),
    IdleMovement (IdleMovement),
    IdlePoint (IdlePoint),
    Position (Position),
    Rng (Rng),
    TargetPosition (TargetPosition),
    Velocity (Velocity),
    Villager (Villager),
  )
import Engine.DataTypes (EntityState (..))
import Engine.Utils (normalizeVectorF, vectorLengthF)
import Engine.Villagers.Tasks (checkCarryDestination, checkEmptyBackpack, checkFilledBackpack, checkForHaulTask, checkPickupDestination, reachedDeliveryDestination, reachedPickupDestination)
import Engine.World (System')
import Linear (V2 (V2))
import System.Random (StdGen, uniformR)

updateVillagers :: Float -> System' ()
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
idleTick dT = cmap $
  \(Villager state, IdleMovement radius baseT idleT) ->
    case state of
      Idle ->
        if idleT > 0
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
checkIdleTimer dt = cmap $
  \(Villager state, IdleMovement radius baseT idleT, IdlePoint ip, TargetPosition mPos, Rng rng) -> case state of
    Idle ->
      if idleT <= 0.0
        then (IdleMovement radius baseT baseT, TargetPosition newTarget, Rng rng')
        else (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)
      where
        (rng', newTarget) = getNewTarget rng ip radius
    _ -> (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)

updateVillagerCollisions :: Float -> System' ()
updateVillagerCollisions _ =
  cmap $ \(Villager _, Position (V2 x y), BoundingBox (V2 rx ry) size) -> BoundingBox (V2 x y) size
