module Engine.Buildings.Spawn (
    spawnFinishedHouse,
    spawnHouseConstruction,
    spawnPlacedHouse,
    spawnStorage
    ) where
import Linear (V2)
import Engine.World (System')
import Apecs
import Engine.DataTypes (StorageItem, StructureState (..))
import Engine.Components
import Engine.Constants
import Linear.V2 (V2(V2))

spawnFinishedHouse :: V2 Float -> System' Entity
spawnFinishedHouse pos = do
  newEntity (
      Building Enabled,
      EntityName "House",
      HaulRequest ("Wood", 60) 0,
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      StorageSpace [],
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      Position pos)

spawnPlacedHouse :: V2 Float -> System' Entity
spawnPlacedHouse pos = do
  newEntity (
      Building Construction,
      EntityName "House",
      HaulRequest ("Wood", 60) 0,
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      StorageSpace [],
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      Position pos)

spawnHouseConstruction :: V2 Float -> System' Entity
spawnHouseConstruction pos = do
  newEntity (
      Building Placement,
      (EntityName "House",
      HaulRequest ("Wood", 60) 0,
      Sprite (V2 (1 * tileSize) (2 * tileSize)) defaultRectSize 1,
      StorageSpace [],
      ConstructionMaterials [("Wood", 60)] [],
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      Position pos))

spawnStorage :: V2 Float -> [StorageItem] -> StructureState -> System' ()
spawnStorage pos storage state = do
  newEntity (
      Building state,
      EntityName "Storage",
      Sprite (V2 (6 * tileSize) (4 * tileSize)) defaultRectSize 1,
      BoundingBox pos defaultRectSizeV2,
      InteractionBox pos defaultRectSizeV2,
      StorageSpace storage,
      Position pos)
  return ()