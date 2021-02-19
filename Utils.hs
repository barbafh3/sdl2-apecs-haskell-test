module Utils (if') where

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
