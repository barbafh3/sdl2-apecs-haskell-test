{-# LANGUAGE TypeApplications #-}

module Engine.Tasks where

import Apecs (cfoldM_, global, set)
import Data.Foldable (traverse_)
import Engine.Components (TaskManager (TaskManager))
import Engine.DataTypes (Build, Haul (Haul))
import Engine.Utils (gget, (</-\>))
import Engine.World (System')

runAllTasks :: System' ()
runAllTasks = do
  (TaskManager haulList buildList) <- gget @TaskManager
  newHaulList <- runAllHaulTask haulList
  newBuildList <- runAllBuildTask buildList
  set global $ TaskManager newHaulList newBuildList

runAllHaulTask :: [Haul] -> System' [Haul]
runAllHaulTask [] = pure []
runAllHaulTask [haul@(Haul etyList required current)]
  | required == current = pure []
  | otherwise = do
    newHaulTask <- runHaulTask haul
    pure [newHaulTask]
runAllHaulTask (haul@(Haul etyList required current) : list)
  | required == current = runAllHaulTask list
  | otherwise = do
    newHaulTask <- runHaulTask haul
    newHaulList <- runAllHaulTask list
    pure $ newHaulTask : newHaulList

runAllBuildTask :: [Build] -> System' [Build]
runAllBuildTask [] = pure []
runAllBuildTask [build] = pure []
runAllBuildTask (build : list) = pure []

runHaulTask :: Haul -> System' Haul
runHaulTask haul@(Haul etyList required current) = do
  let remaining = required </-\> current
  pure haul