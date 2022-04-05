{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Engine.Particles (spawnParticles, updateParticles, updateParticlePositions) where

import Apecs
  ( Component (Storage),
    Has,
    Not (Not),
    SystemT,
    cmap,
    get,
    global,
    liftIO,
    newEntity,
  )
import qualified Apecs.Core
import Control.Monad (replicateM_)
import Engine.Components
  ( Kinetic,
    MousePosition (..),
    Particle (..),
    Position (Position),
    Velocity (Velocity),
  )
import Engine.World (System')
import Linear (V2 (V2))
import SDL (getRelativeMouseLocation)
import qualified SDL
import System.Random (randomIO, randomRIO)

spawnParticles :: Int -> System' ()
spawnParticles amount = do
  MousePosition (V2 x y) <- gget @MousePosition
  -- (SDL.P mPos) <- SDL.getAbsoluteMouseLocation
  -- (V2 x y) <- return $ fromIntegral <$> mPos
  (V2 x y) <- return $ V2 640 450
  replicateM_ amount $ do
    rand <- randomIO
    vx <- if (rand :: Float) > 0.5 then randomRIO (0, 1) else (* (-1)) <$> randomRIO (0, 1)
    vy <- liftIO $ randomRIO (-2, -3)
    t <- liftIO $ randomRIO (10, 14)
    newEntity (Particle t, Position (V2 x y), Velocity (V2 vx vy))

updateParticles :: Float -> System' ()
updateParticles dT = cmap $ \(Particle t) ->
  if t < 0
    then Right $ Not @(Particle, Kinetic)
    else Left $ Particle (t - 0.1)

gget :: forall c w m. (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c
gget = Apecs.get global

updateParticlePositions :: Float -> System' ()
updateParticlePositions dT = cmap $ \(Particle t, Position p, Velocity (V2 vx vy)) -> (Position (p + V2 vx vy), Velocity (V2 vx (vy + 0.1)))
