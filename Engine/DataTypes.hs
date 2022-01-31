module Engine.DataTypes (
    StorageItem(..), StorageList(..), DrawLevels(..), EntityState(..),
    FontMap, FontResource, Hover, Clicked, Toggled, Offset, ClickState(..),
    StructureState(..)
) where
import SDL.Font (Font)
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types (CInt)

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

data ClickState = Clicked | ClickReleased | ClickHeld | NotClicked deriving Show

data StructureState = Placement | Construction | Enabled | Disabled deriving Show

