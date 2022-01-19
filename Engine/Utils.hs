{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Engine.Utils (
    (<+>), (<->), 
    vectorLength, normalizeVector, 
    vectorLengthF, normalizeVectorF, 
    gget, truncate', loadFonts, createResourceMap
) where

import Apecs (get, global)
import Apecs.Core
import Linear (V2(V2))
import Engine.Components (Position (Position))
import Foreign.C (CInt)
import SDL.Font (PointSize, load)
import Engine.DataTypes (FontResource)
import Data.HashMap.Strict (insert, empty, HashMap)

sumV2 :: V2 CInt -> Float
sumV2 (V2 x y) = fromIntegral (x + y)

-- | Adds both vectors
(<+>) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <+> (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

-- | Subtracts the second vector form the first vector
(<->) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <-> (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

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

gget :: forall c w m . (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c
gget = get global

-- translate' :: Position -> Picture -> Picture
-- translate' (Position (V2 x y)) = translate x y

truncate' decimals number = let
    toFloat n  = read n :: Float
    totalChars = (+) (decimals+1) $ getPos '.' (show number) 0
        where getPos c (x:xs) n
                  | x == c    = n
                  | otherwise = getPos c xs n+1
    in toFloat $ take totalChars $ show number

loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where getFont (p, s) = do
          font <- load p s
          pure (p, font)

createResourceMap :: [(String, a)] -> HashMap String a
createResourceMap = foldl (\m (k, v) -> insert k v m) empty
