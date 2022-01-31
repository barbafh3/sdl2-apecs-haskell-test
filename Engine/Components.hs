{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Engine.Components(
    Position(..), TargetPosition(..), IdlePoint(..),
    Velocity(..), Kinetic(..), IdleMovement(..),
    Particle(..),
    MousePosition(..), Rng(..),
    Villager(..), Hauler(..), Builder(..),
    Building(..), StorageSpace(..), Backpack(..),
    BoundingBox(..), InteractionBox(..),
    DrawLevel(..), InfoPanel(..),
    EntityName(..), Origin(..), Destination(..),
    HaulTask(..), HaulRequest(..),
    Sprite(..), UIText(..), Fonts(..),
    Button(..), InterfaceBox(..),
    HouseButton(..), SelectedConstruction(..),
) where
import Apecs
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Engine.DataTypes (
    StorageList, DrawLevels (Default),
    EntityState, StorageItem, FontMap,
    ClickState(..), Hover, Toggled, Offset, StructureState(..))
import SDL (Texture)
import Foreign.C (CInt)
import SDL.Video
import SDL.Font (Font)
import qualified Data.HashMap.Strict as HM


------------------------- ASSETS ---------------------------

newtype Fonts = Fonts FontMap deriving Show
instance Semigroup Fonts where (<>) = mappend
instance Monoid Fonts where mempty = Fonts HM.empty
instance Component Fonts where type Storage Fonts = Global Fonts

newtype Rng = Rng StdGen deriving Show
instance Semigroup Rng where (<>) = mappend
instance Monoid Rng where mempty = Rng $ mkStdGen 1
instance Component Rng where type Storage Rng = Global Rng

------------------------- BUILDINGS ------------------------

newtype Building = Building StructureState deriving Show
instance Component Building where type Storage Building = Map Building

newtype StorageSpace = StorageSpace StorageList deriving Show
instance Component StorageSpace where type Storage StorageSpace = Map StorageSpace

------------------------- CORE -----------------------------

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)

newtype TargetPosition = TargetPosition (V2 Float) deriving Show
instance Component TargetPosition where type Storage TargetPosition = Map TargetPosition

newtype Particle = Particle Float deriving Show
instance Component Particle where type Storage Particle = Map Particle

newtype MousePosition = MousePosition (V2 Float) deriving Show
instance Semigroup MousePosition where (<>) = mappend
instance Monoid MousePosition where mempty = MousePosition (V2 0 0)
instance Component MousePosition where type Storage MousePosition = Global MousePosition

data BoundingBox = BoundingBox (V2 Float) (V2 Float) deriving Show
instance Component BoundingBox where type Storage BoundingBox = Map BoundingBox

data InteractionBox = InteractionBox (V2 Float) (V2 Float) deriving Show
instance Component InteractionBox where type Storage InteractionBox = Map InteractionBox

newtype EntityName = EntityName String deriving Show
instance Component EntityName where type Storage EntityName = Map EntityName

data Sprite = Sprite (V2 CInt) (V2 CInt) Float deriving Show
instance Component Sprite where type Storage Sprite = Map Sprite

------------------------- DEBUG ----------------------------

newtype InfoPanel = InfoPanel (Maybe Entity) deriving Show
instance Semigroup InfoPanel where (<>) = mappend
instance Monoid InfoPanel where mempty = InfoPanel Nothing
instance Component InfoPanel where type Storage InfoPanel = Global InfoPanel

newtype DrawLevel = DrawLevel DrawLevels deriving (Show, Eq)
instance Semigroup DrawLevel where (<>) = mappend
instance Monoid DrawLevel where mempty = DrawLevel Default
instance Component DrawLevel where type Storage DrawLevel = Global DrawLevel

------------------------- Global ---------------------------

newtype SelectedConstruction = SelectedConstruction (Maybe Entity) deriving Show
instance Semigroup SelectedConstruction where (<>) = mappend
instance Monoid SelectedConstruction where mempty = SelectedConstruction Nothing
instance Component SelectedConstruction where type Storage SelectedConstruction = Global SelectedConstruction

------------------------- TAGS -----------------------------

data HouseButton = HouseButton deriving Show
instance Component HouseButton where type Storage HouseButton = Map HouseButton

------------------------- TASKS ----------------------------

data HaulRequest = HaulRequest StorageItem Int deriving Show
instance Component HaulRequest where type Storage HaulRequest = Map HaulRequest

data ConstructRequest = ConstructRequest deriving Show
instance Component ConstructRequest where type Storage ConstructRequest = Map ConstructRequest

newtype Origin = Origin Int deriving Show
instance Component Origin where type Storage Origin = Map Origin

newtype Destination = Destination Int deriving Show
instance Component Destination where type Storage Destination = Map Destination

data HaulTask = HaulTask StorageItem Int Int deriving Show
instance Component HaulTask where type Storage HaulTask = Map HaulTask

data BuildTask = BuildTask StorageItem Int Int deriving Show
instance Component BuildTask where type Storage BuildTask = Map BuildTask

------------------------- UI ------------------------------

data Button = Button ClickState Hover Toggled deriving Show
instance Component Button where type Storage Button = Map Button

newtype InterfaceBox = InterfaceBox (V2 Float) deriving Show
instance Component InterfaceBox where type Storage InterfaceBox = Map InterfaceBox

newtype UIText = UIText String deriving Show
instance Component UIText where type Storage UIText = Map UIText

------------------------- VILLAGERS ------------------------

newtype Villager = Villager EntityState deriving Show
instance Component Villager where type Storage Villager = Map Villager

data Hauler = Hauler deriving Show
instance Component Hauler where type Storage Hauler = Map Hauler

data Builder = Builder deriving Show
instance Component Builder where type Storage Builder = Map Builder

data IdleMovement = IdleMovement Float Float Float deriving Show
instance Component IdleMovement where type Storage IdleMovement = Map IdleMovement

newtype IdlePoint = IdlePoint (V2 Float) deriving Show
instance Component IdlePoint where type Storage IdlePoint = Map IdlePoint

newtype Backpack = Backpack (Maybe StorageItem) deriving Show
instance Component Backpack where type Storage Backpack = Map Backpack
