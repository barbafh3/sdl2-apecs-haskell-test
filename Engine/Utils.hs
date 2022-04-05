{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Engine.Utils
  ( (<+>),
    (<->),
    (<#>),
    (</-\>),
    vectorLength,
    normalizeVector,
    vectorLengthF,
    normalizeVectorF,
    gget,
    truncate',
    loadFonts,
    createResourceMap,
    getRelativeBoxPosition,
    cfoldMap,
    removeFromList,
  )
where

import Apecs (get, global)
import Apecs.Core
  ( Component (Storage),
    ExplGet,
    Get,
    Has,
    Members,
    SystemT,
  )
import Apecs.System (cfold)
import Data.HashMap.Strict (HashMap, empty, insert)
import Engine.Components (Position (Position))
import Engine.DataTypes (FontResource, StorageItem, StorageList)
import Foreign.C (CInt)
import Linear (V2 (V2))
import SDL.Font (PointSize, load)

sumV2 :: V2 CInt -> Float
sumV2 (V2 x y) = fromIntegral (x + y)

-- | Adds both vectors
(<+>) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <+> (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

-- | Subtracts the second vector form the first vector
(<->) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <-> (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

-- | Substracts two storage lists
(</-\>) :: StorageList -> StorageList -> StorageList
[] </-\> [] = []
[] </-\> [item] = []
[] </-\> (item : list) = []
[item] </-\> [] = [item]
fullList@(item : list) </-\> [] = fullList
[item1@(resource1, amount1)] </-\> [item2@(resource2, amount2)]
  | resource1 == resource2 = [(resource1, amount1 - amount2)]
  | otherwise = [item1]
[item1@(resource1, amount1)] </-\> (item2@(resource2, amount2) : list)
  | resource1 == resource2 = (resource1, amount1 - amount2) : list
  | otherwise = [item1] </-\> list
(item1@(resource1, amount1) : list1) </-\> (item2@(resource2, amount2) : list2)
  | resource1 == resource2 = (resource2, amount1 - amount2) : list1 </-\> list2
  | otherwise = item1 : list1 </-\> list2

-- | Substracts two storage lists
(</+\>) :: StorageList -> StorageList -> StorageList
[] </+\> [] = []
[] </+\> [item] = []
[] </+\> (item : list) = []
[item] </+\> [] = [item]
fullList@(item : list) </+\> [] = fullList
[item1@(resource1, amount1)] </+\> [item2@(resource2, amount2)]
  | resource1 == resource2 = [(resource1, amount1 + amount2)]
  | otherwise = [item1]
[item1@(resource1, amount1)] </+\> (item2@(resource2, amount2) : list)
  | resource1 == resource2 = (resource1, amount1 + amount2) : list
  | otherwise = [item1] </+\> list
(item1@(resource1, amount1) : list1) </+\> (item2@(resource2, amount2) : list2)
  | resource1 == resource2 = (resource2, amount1 + amount2) : list1 </+\> list2
  | otherwise = item1 : list1 </+\> list2

vectorLength :: V2 CInt -> Float
vectorLength vec = sqrt . sumV2 $ (^ 2) <$> vec

normalizeVector :: V2 CInt -> V2 CInt
normalizeVector vec@(V2 x y) = (`div` round (vectorLength vec)) <$> vec

sumV2F :: V2 Float -> Float
sumV2F (V2 x y) = x + y

vectorLengthF :: V2 Float -> Float
vectorLengthF vec = sqrt . sumV2F $ (** 2) <$> vec

normalizeVectorF :: V2 Float -> V2 Float
normalizeVectorF vec@(V2 x y) = (/ vectorLengthF vec) <$> vec

gget :: forall c w m. (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c
gget = get global

-- translate' :: Position -> Picture -> Picture
-- translate' (Position (V2 x y)) = translate x y

truncate' decimals number =
  let toFloat n = read n :: Float
      totalChars = (+) (decimals + 1) $ getPos '.' (show number) 0
        where
          getPos c (x : xs) n
            | x == c = n
            | otherwise = getPos c xs n + 1
   in toFloat $ take totalChars $ show number

loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where
    getFont (p, s) = do
      font <- load p s
      pure (p, font)

createResourceMap :: [(String, a)] -> HashMap String a
createResourceMap = foldl (\m (k, v) -> insert k v m) empty

getRelativeBoxPosition :: V2 CInt -> V2 CInt -> V2 CInt -> (V2 CInt, V2 CInt)
getRelativeBoxPosition (V2 x y) (V2 w h) (V2 sw sh) = (pos1, pos2)
  where
    (V2 sdx sdy) = V2 ((w - sw) `div` 2) ((h - sh) `div` 2)
    pos1@(V2 p1x p1y) = V2 (x - sdx) (y - sdy)
    pos2 = V2 (p1x + w) (p1y + h)

-- | Maps the function to the values of a V2 CInt, returning a V2 Float
(<#>) :: (CInt -> Float) -> V2 CInt -> V2 Float
f <#> (V2 x y) = V2 (f x) (f y)

cfoldMap :: forall w m c a. Members w m c => Get w m c => Monoid a => (c -> a) -> SystemT w m a
cfoldMap f = cfold (\acc c -> mappend acc (f c)) mempty

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList element = filter (/= element)
