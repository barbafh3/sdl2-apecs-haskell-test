module Engine.Buildings.Storage
  ( emptyStorageList,
    addToStorage,
    removeFromStorage,
    totalUsage,
    resourceCount,
    checkResourceInStorage,
  )
where

import Engine.DataTypes (StorageItem, StorageList)

emptyStorageList :: StorageList
emptyStorageList = [("Wood", 0)]

addToStorage :: StorageItem -> StorageList -> StorageList
addToStorage item [] = [item]
addToStorage item [pair]
  | fst pair == fst item = [(fst pair, snd pair + snd item)]
  | otherwise = addToStorage item []
addToStorage item (pair : list)
  | fst pair == fst item = (fst pair, snd pair + snd item) : list
  | otherwise = addToStorage item list

removeFromStorage :: Maybe StorageItem -> StorageList -> StorageList
removeFromStorage Nothing [] = []
removeFromStorage (Just item) [] = [item]
removeFromStorage (Just item) [pair]
  | fst pair == fst item =
    if snd pair - snd item <= 0
      then [(fst pair, 0)]
      else [(fst pair, snd pair - snd item)]
  | otherwise = [pair]
removeFromStorage (Just item) (pair : list)
  | fst pair == fst item =
    if snd pair - snd item <= 0
      then (fst pair, 0) : removeFromStorage (Just item) list
      else (fst pair, snd pair - snd item) : removeFromStorage (Just item) list
  | otherwise = removeFromStorage (Just item) list
removeFromStorage Nothing (pair : list) = pair : list

totalUsage :: StorageList -> Int
totalUsage [] = 0
totalUsage [pair] = snd pair
totalUsage (pair : list) = snd pair + totalUsage list

resourceCount :: String -> StorageList -> Int
resourceCount _ [] = 0
resourceCount resource [pair]
  | fst pair == resource = snd pair
  | otherwise = 0
resourceCount resource (pair : list)
  | fst pair == resource = snd pair + resourceCount resource list
  | otherwise = resourceCount resource list

checkResourceInStorage :: StorageList -> StorageItem -> Bool
checkResourceInStorage [] _ = False
checkResourceInStorage [(sResource, sAmount)] (resource, amount)
  | sResource == resource && sAmount >= amount = True
  | otherwise = False
checkResourceInStorage ((sResource, sAmount) : list) item@(resource, amount)
  | sResource == resource && sAmount >= amount = True
  | sResource == resource && sAmount < amount = False
  | otherwise = checkResourceInStorage list item
