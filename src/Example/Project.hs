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

forceApplicator :: DSignal dom d (Bodies (2 ^ k), Body) -> DSignal dom (d + k) Body
forceApplicator inputSignal = applyForce <$> delayI targetSignal <*> totalForceSignal
  where
    (sourcesSignal, targetSignal) = unbundle inputSignal
    foldSignals = bundle (,targetSignal) <$> unbundle sourcesSignal
    totalForceSignal = delayedFold d1 (V3 0 0 0) (\acc (source, target) -> acc + forceFor target source) foldSigs

    applyForce :: Body -> FixedV3 -> Body
    applyForce target force = target { velocity = velocity target + force }

    forceFor :: Body -> FixedV3
    forceFor target source = g * mass target * mass source *^ r ^/ dotx
      where
        dotx :: MyFixed
        dotx = (position source `dot` position target) + dampening
        r :: FixedV3
        r = (position source - position target) ^/ sqrtC d10 dotx

velocityApplicator :: DSignal dom d Body -> DSignal dom d Body
velocityApplicator = fmap applyVelocity
  where
    applyVelocity :: Body -> Body
    applyVelocity body = body { position = position body + velocity body }

gravityApplicator :: DSignal dom d (Bodies (2 ^ k), Body) -> DSignal dom (d + k) Body
gravityApplicator = velocityApplicator . forceApplicator

multiGravityApplicator :: DSignal dom d (Bodies (2 ^ k)) -> DSignal dom (d + k) (Bodies (2 ^ k))
multiGravityApplicator bodiesSignal = traverse (gravityApplicator . bundle . (bodiesSignal,)) . unbundle $ bodiesSignal

nBodyIntegrator :: SNat (2 ^ k) -> Signal dom d (Bodies (2 ^ k)) -> Signal dom (d + k) (Bodies (2 ^ k))
nBodyIntegrator n bodiesSignal = output
  where
    isFirst = register True (const False <$> isFirst)
    shouldEmit = (==0) <$> moore (\s _ -> succ s % n) id 0

-- Integrate using a Moore machine
nbodyIntegrator :: (HiddenClockResetEnable dom, KnownNat n, 1 <= n) => Signal dom (Maybe (Bodies n)) -> Signal dom (Maybe (Bodies n))
nbodyIntegrator input = output
  (moore step id Nothing
  where
    step :: (KnownNat n, 1 <= n) => Maybe (Bodies n) -> Maybe (Bodies n) -> Maybe (Bodies n)
    step Nothing Nothing = Nothing
    step Nothing (Just input) = Just input
    step (Just bodies) _ = Just . fmap (applyVelocity . applyForces bodies) $ bodies

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