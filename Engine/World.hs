{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Apecs
  ( Component (Storage),
    Has (..),
    System,
    SystemT (SystemT),
    asks,
    explInit,
    makeWorld,
  )
import Engine.Components
  ( Backpack,
    BoundingBox,
    Builder,
    Building,
    Button,
    ConstructionMaterials,
    ConstructionStorage,
    Destination,
    DrawLevel,
    EntityName,
    Fonts,
    HaulRequest,
    HaulTask,
    Hauler,
    HouseButton,
    IdleMovement,
    IdlePoint,
    InfoPanel,
    InteractionBox,
    InterfaceBox,
    MousePosition,
    Origin,
    Particle,
    Position,
    Rng,
    SelectedConstruction,
    Sprite,
    StorageSpace,
    TargetPosition,
    TaskManager,
    UIText,
    Velocity,
    Villager,
  )
import Engine.DataTypes (StructureState)

makeWorld
  "World"
  [ ''Position,
    ''Velocity,
    ''Particle,
    ''MousePosition,
    ''Villager,
    ''IdleMovement,
    ''TargetPosition,
    ''IdlePoint,
    ''Rng,
    ''Sprite,
    ''Building,
    ''StorageSpace,
    ''BoundingBox,
    ''DrawLevel,
    ''InteractionBox,
    ''InfoPanel,
    ''EntityName,
    ''Hauler,
    ''Origin,
    ''Destination,
    ''Builder,
    ''Backpack,
    ''HaulTask,
    ''HaulRequest,
    ''Button,
    ''Fonts,
    ''UIText,
    ''InterfaceBox,
    ''HouseButton,
    ''SelectedConstruction,
    ''ConstructionMaterials,
    ''ConstructionStorage,
    ''TaskManager
  ]

type System' a = System World a
