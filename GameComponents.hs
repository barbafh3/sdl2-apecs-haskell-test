{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GameComponents (
    Position(..), 
    Sprite(..),
    Speed(..),
    Player(..), 
    Name(..),
    Tile(..),
    Tilemap
    ) where

import Apecs
import Linear (V2(..))
import Foreign.C.Types

newtype Position = Position (V2 CInt) deriving (Show)
instance Component Position where type Storage Position = Map Position

data Sprite = Sprite {
    coord :: V2 CInt,
    rect :: V2 CInt
    }
instance Component Sprite where type Storage Sprite = Map Sprite

newtype Speed = Speed CInt deriving (Show)
instance Component Speed where type Storage Speed = Map Speed

data Player = Player deriving (Show)
instance Component Player where type Storage Player = Unique Player

newtype Name = Name String deriving (Show)
instance Component Name where type Storage Name = Map Name

data Tile = Tile deriving (Show)
instance Component Tile where type Storage Tile = Map Tile

type Tilemap = [Sprite]
