{-# LANGUAGE FlexibleContexts #-}

module Example.Project (topEntity, nbodyIntegrator, fullCircuit, emptyBody, initial, position, velocity, mass) where

import Clash.Prelude
import Example.Math
import Example.Memory
import Example.Util
import Example.V3

type MyFixed = SFixed 16 8
type FixedV3 = V3 MyFixed

data Body = Body { mass :: MyFixed
                 , position :: FixedV3
                 , velocity :: FixedV3
                 } deriving (Generic,NFDataX,Show)
type Bodies (n :: Nat) = Vec n Body

emptyBody :: Body
emptyBody = Body { mass = 0.0, position = V3 0.0 0.0 0.0, velocity = V3 0.0 0.0 0.0 }

g :: MyFixed
g = 6.673e-11

dampening :: MyFixed
dampening = 0.1

-- Performance can be improved by skipping self-acting forces since these will always be 0.
-- Note that this is a simple Euler integrator and thus not very accurate.
applyForces :: (KnownNat n, 1 <= n) => Bodies n -> Body -> Body
applyForces sources target = target { velocity = velocity target + sum (map forceFor sources) }
  where
    forceFor :: Body -> FixedV3
    forceFor source = g * mass target * mass source *^ r ^/ dotx
      where
        dotx :: MyFixed
        dotx = (position source `dot` position target) + dampening
        r :: FixedV3
        r = (position source - position target) ^/ sqrtC d10 dotx

applyVelocity :: Body -> Body
applyVelocity body = body { position = position body + velocity body }

-- Integrate using a Moore machine
nbodyIntegrator :: (HiddenClockResetEnable dom, KnownNat n, 1 <= n) => Signal dom (Maybe (Bodies n)) -> Signal dom (Maybe (Bodies n))
nbodyIntegrator = moore step id Nothing
  where
    step :: (KnownNat n, 1 <= n) => Maybe (Bodies n) -> Maybe (Bodies n) -> Maybe (Bodies n)
    step Nothing Nothing = Nothing
    step Nothing (Just input) = Just input
    step (Just bodies) _ = Just . map applyVelocity . map (applyForces bodies) $ bodies

inputMemory :: (HiddenClockResetEnable dom, KnownNat n) => Bodies n -> Blockram dom n Body
inputMemory inputData = blockRam inputData

outputMemory :: (HiddenClockResetEnable dom, KnownNat n, KnownNat s, 1 <= s) => SNat s -> Blockram dom s (Bodies n)
outputMemory n = blockRamU NoClearOnReset n undefined

initial :: Bodies 2
initial
  =  Body { mass = 0.3, position = V3 0.3 0.5 0.3, velocity = V3 1.0 0.2 0.4 }
  :> Body { mass = 2.4, position = V3 2.0 4.3 2.9, velocity = V3 4.3 5.3 2.1 }
  :> Nil

fullCircuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom (Maybe (Bodies 2))
fullCircuit _ = nbodyIntegrator $ memoryLoader (inputMemory initial)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System ()
  -> Signal System (Maybe (Bodies 2))
topEntity = exposeClockResetEnable fullCircuit