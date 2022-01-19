module Engine.DataTypes (
    StorageItem(..), StorageList(..), DrawLevels(..), EntityState(..),
    FontMap, FontResource
) where
import SDL.Font (Font)
import qualified Data.HashMap.Strict as HM

type StorageItem = (String, Int)

type StorageList = [StorageItem]

data DrawLevels = Default | Collision | Particles | All | Debug deriving (Show, Eq)

data EntityState = Idle | Carrying | Loading | Constructing | Enabled | Disabled deriving (Show, Eq)

type FontResource = (String, Font)
type FontMap = HM.HashMap String Font

