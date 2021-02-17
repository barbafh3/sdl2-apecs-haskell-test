{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameSystem where

import Apecs
import GameComponents

makeWorld "World" [''Position, ''Speed, ''Sprite, ''Player, ''Name, ''Tile]

type System' a = System World a
