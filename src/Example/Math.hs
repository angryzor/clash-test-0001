module Example.Math (sqrtC) where

import Clash.Prelude

-- Iteration of Newton's method
improveSqrtGuess :: Fractional a => a -> a -> a
improveSqrtGuess x guess = (guess + x / guess) / 2

-- Newton's method as a chain of n combinational circuits
sqrtC :: (KnownNat n, Fractional a) => SNat n -> a -> a
sqrtC n x = generate n (improveSqrtGuess x) 1.0 !! (snatToNatural n - 1)

-- -- Newton's method as a mealy machine
-- sqrtT :: (Fractional a, Ord a) => a -> a -> (a, a)
-- sqrtT guess x
--   | abs (guess * guess - x) < 0.1 = (guess, guess)
--   | otherwise                     = (improveSqrtGuess x guess, guess)

-- sqrtS :: (NFDataX a, Fractional a, Ord a, HiddenClockResetEnable dom) => Signal dom a -> Signal dom a
-- sqrtS = mealy sqrtT 1.0

-- sqrtCT :: (HiddenClockResetEnable dom, Fractional a) => Signal dom a -> Signal dom a
-- sqrtCT = mealy (\_ x -> (0 :: Signed 8, sqrtC d10 x)) 0
