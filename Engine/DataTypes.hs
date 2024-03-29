module Engine.DataTypes
  ( StorageItem (..),
    StorageList (..),
    DrawLevels (..),
    EntityState (..),
    FontMap,
    FontResource,
    Hover,
    Clicked,
    Toggled,
    Offset,
    ClickState (..),
    StructureState (..),
    GameTextures (..),
    Haul (..),
    Build (..),
  )
where

import Apecs (Entity)
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types (CInt)
import qualified SDL
import SDL.Font (Font)

type Hover = Bool

type Clicked = Bool

type Toggled = Bool

type Offset = CInt

type StorageItem = (String, Int)

type StorageList = [StorageItem]

data DrawLevels = Default | Collision | Particles | All | Debug deriving (Show, Eq)

data EntityState = Idle | Carrying | Loading deriving (Show, Eq)

type FontResource = (String, Font)

type FontMap = HM.HashMap String Font

data ClickState = Clicked | ClickReleased | ClickHeld | NotClicked deriving (Show)

data StructureState = Placement | Construction | Enabled | Disabled deriving (Show)

type GameTextures = (SDL.Texture, SDL.Texture, SDL.Texture)

-------------- TESTING ---------------

data Haul = Haul [Entity] StorageList StorageList deriving (Show)

newtype Build = Build [Entity] deriving (Show)
