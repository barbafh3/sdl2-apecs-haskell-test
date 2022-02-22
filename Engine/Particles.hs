{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Engine.Particles (spawnParticles, updateParticles, updateParticlePositions) where

import Engine.Components
import Linear
import Apecs
import System.Random
import Control.Monad
import qualified Apecs.Core
import Engine.World (System')
import SDL (getRelativeMouseLocation)
import qualified SDL

spawnParticles :: Int -> System' ()
spawnParticles amount = do
  MousePosition (V2 x y) <- gget @MousePosition
  -- (SDL.P mPos) <- SDL.getAbsoluteMouseLocation
  -- (V2 x y) <- return $ fromIntegral <$> mPos
  (V2 x y) <- return $ V2 640 450
  replicateM_ amount $ do
    rand <- randomIO
    vx <- if (rand :: Float) > 0.5 then randomRIO (0, 1) else (*(-1)) <$> randomRIO (0, 1)
    vy <- liftIO $ randomRIO (-2, -3)
    t  <- liftIO $ randomRIO (10, 14)
    newEntity (Particle t, Position (V2 x y), Velocity (V2 vx vy))

updateParticles :: Float -> System' ()
updateParticles dT = cmap $ \(Particle t) ->
  if t < 0
     then Right $ Not @(Particle, Kinetic)
     else Left  $ Particle (t - 0.1)


gget :: forall c w m . (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c
gget = Apecs.get global

updateParticlePositions :: Float -> System' ()
updateParticlePositions dT = cmap $ \(Particle t, Position p, Velocity (V2 vx vy)) -> (Position (p + V2 vx vy), Velocity (V2 vx (vy + 0.1)))

