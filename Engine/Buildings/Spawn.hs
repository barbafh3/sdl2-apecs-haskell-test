module Engine.Buildings.Spawn
  ( spawnFinishedHouse,
    spawnHouseConstruction,
    spawnPlacedHouse,
    spawnFinishedStorage,
  )
where

import Apecs (Entity, newEntity)
import Engine.Components
  ( BoundingBox (BoundingBox),
    Building (Building),
    ConstructionMaterials (ConstructionMaterials),
    ConstructionStorage (ConstructionStorage),
    EntityName (EntityName),
    HaulRequest (HaulRequest),
    InteractionBox (InteractionBox),
    Position (Position),
    Sprite (Sprite),
    StorageSpace (StorageSpace),
  )
import Engine.Constants
  ( defaultRectSize,
    defaultRectSizeV2,
    tileSize,
  )
import Engine.DataTypes (StorageItem, StructureState (..))
import Engine.World (System')
import Linear (V2)
import Linear.V2 (V2 (V2))

spawnFinishedHouse :: V2 Float -> System' Entity
spawnFinishedHouse pos = do
  newEntity
    ( Building Enabled,
      EntityName "House",
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      Position pos
    )

spawnPlacedHouse :: V2 Float -> System' Entity
spawnPlacedHouse pos = do
  newEntity
    ( Building Construction,
      EntityName "House",
      HaulRequest [("Wood", 60)] [],
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      ConstructionStorage [],
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      Position pos
    )

spawnHouseConstruction :: V2 Float -> System' Entity
spawnHouseConstruction pos = do
  newEntity
    ( Building Placement,
      ( EntityName "House",
        HaulRequest [("Wood", 60)] [],
        Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
        ConstructionStorage [],
        ConstructionMaterials [("Wood", 60)] [],
        BoundingBox pos defaultRectSizeV2,
        InteractionBox pos defaultRectSizeV2,
        Position pos
      )
    )

spawnFinishedStorage :: V2 Float -> [StorageItem] -> StructureState -> System' ()
spawnFinishedStorage pos storage state = do
  newEntity
    ( Building state,
      EntityName "Storage",
      Sprite (V2 (6 * tileSize) (4 * tileSize)) defaultRectSize 1,
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      StorageSpace storage,
      Position pos
    )
  return ()
