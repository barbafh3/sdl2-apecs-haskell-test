module Engine.Villagers.Spawn (
    spawnHauler
    ) where
import Linear (V2)
import Engine.World (System')
import Engine.Components
import Apecs
import Engine.DataTypes (EntityState(..))
import Linear.V2 (V2(V2))
import Engine.Constants

spawnHauler :: V2 Float -> V2 Float -> V2 Float -> System' ()
spawnHauler pos idlePoint vel = do
  newEntity (
    Hauler,
    (Villager Idle,
    (Backpack Nothing,
    (BoundingBox pos (V2 8 8),
    (IdleMovement 20 3.0 0.0,
    (IdlePoint idlePoint,
    (Position pos,
    (Velocity vel,
    (Sprite (V2 (6 * tileSize) (12 * tileSize)) defaultRectSize 1,
    TargetPosition (V2 0 0))))))))))
  return ()
