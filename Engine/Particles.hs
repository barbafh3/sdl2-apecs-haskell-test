{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Engine.Particles (spawnParticles, stepParticles, stepParticlePositions) where

import Engine.Components
import Linear
import Apecs
import System.Random
import Control.Monad
import qualified Apecs.Core
import Engine.World (System')

spawnParticles :: Int -> System' ()
spawnParticles amount = do
  MousePosition (V2 x y) <- gget @MousePosition
  replicateM_ amount $ do
    rand <- randomIO
    vx <- if (rand :: Float) > 0.5 then randomRIO (0, 40) else (*(-1)) <$> randomRIO (0, 40)
    vy <- liftIO $ randomRIO (60, 80)
    t  <- liftIO $ randomRIO (4, 6)
    newEntity (Particle t, Position (V2 x y), Velocity (V2 (vx - 1) vy))

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0
     then Right $ Not @(Particle, Kinetic)
     else Left  $ Particle (t - 0.1)


gget :: forall c w m . (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c
gget = Apecs.get global

stepParticlePositions :: Float -> System' ()
stepParticlePositions dT = cmap $ \(Particle t, Position p, Velocity (V2 vx vy)) -> (Position (p + V2 vx vy), Velocity (V2 vx (vy - 2)))

