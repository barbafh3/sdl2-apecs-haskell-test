{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Engine.World where

import Apecs
import Engine.Components

makeWorld "World" [
    ''Position, ''Velocity, ''Particle, ''MousePosition, ''GlobalUnique, ''Villager,
    ''IdleMovement, ''TargetPosition, ''IdlePoint, ''Rng, ''Sprite, ''Building, ''StorageSpace,
    ''BoundingBox, ''DrawLevel, ''InteractionBox, ''InfoPanel, ''EntityName, ''Hauler, ''Origin,
    ''Destination, '' Builder, ''Backpack, ''HaulTask, ''HaulRequest, ''Button, ''Fonts,
    ''UIText, ''InterfaceBox]

type System' a = System World a
