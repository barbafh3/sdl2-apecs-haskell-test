module Engine.Villagers.Spawn
  ( spawnHauler,
  )
where

import Apecs (newEntity)
import Engine.Components
  ( Backpack (Backpack),
    BoundingBox (BoundingBox),
    Hauler (Hauler),
    IdleMovement (IdleMovement),
    IdlePoint (IdlePoint),
    Position (Position),
    Sprite (Sprite),
    TargetPosition (TargetPosition),
    Velocity (Velocity),
    Villager (Villager),
  )
import Engine.Constants (defaultRectSize, tileSize)
import Engine.DataTypes (EntityState (..))
import Engine.World (System')
import Linear (V2)
import Linear.V2 (V2 (V2))

spawnHauler :: V2 Float -> V2 Float -> V2 Float -> System' ()
spawnHauler pos idlePoint vel = do
  newEntity
    ( Hauler,
      ( Villager Idle,
        ( Backpack Nothing,
          ( BoundingBox pos (V2 8 8),
            ( IdleMovement 20 3.0 0.0,
              ( IdlePoint idlePoint,
                ( Position pos,
                  ( Velocity vel,
                    ( Sprite (V2 (6 * tileSize) (12 * tileSize)) defaultRectSize 1,
                      TargetPosition (V2 0 0)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  return ()
