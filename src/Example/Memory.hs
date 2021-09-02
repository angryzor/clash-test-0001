module Example.Memory (memoryLoader, memoryStreamWriter) where

import Clash.Prelude
import Control.Monad
import Example.Util

data State n = AwaitingFirst | AtIndex (Index n) | Done deriving (Generic,NFDataX)

-- memoryLoader
--   :: HiddenClockResetEnable dom
--   => NFDataX a
--   => KnownNat n
--   => Blockram dom n a
--   -> Signal dom (Maybe (Vec n a))
-- memoryLoader ram = result
--   where
--     memRead = ram memAddr (always Nothing)
--     (memAddr,result) = unbundle (moore step out initState memRead)
--       where
--         initState = (AwaitingFirst, repeat undefined)

--         step (AwaitingFirst, results) _ = (AtIndex 0, results)
--         step (Done, results) _ = (Done, results)
--         step (AtIndex i, results) item
--           | i == maxBound = (Done, replace i item results)
--           | otherwise     = (AtIndex (succ i), replace i item results)

--         out (AwaitingFirst, _) = (0, Nothing)
--         out (Done, results) = (0, Just results)
--         out (AtIndex i, _)
--           | i == maxBound = (0, Nothing)
--           | otherwise     = (succ i, Nothing)

memoryLoader
  :: HiddenClockResetEnable dom
  => NFDataX a
  => KnownNat n
  => Blockram dom n a
  -> Signal dom (Maybe (Vec n a))
memoryLoader ram = result
  where
    memRead = ram readAddr (always Nothing)
    (readAddr,result) = unbundle (mealy step initState memRead)
      where
        initState = (AwaitingFirst, repeat undefined)

        step (AwaitingFirst, results) _ = ((AtIndex 0, results), (0, Nothing))
        step (AtIndex i, results) item
          | i == maxBound = ((AtIndex i, results), (i, Just (replace i item results)))
          | otherwise     = ((AtIndex (succ i), replace i item results), (succ i, Nothing))

memoryStreamWriter
  :: HiddenClockResetEnable dom
  => NFDataX a
  => KnownNat n
  => Blockram dom n a
  -> Signal dom (Maybe a)
  -> Signal dom ()
memoryStreamWriter ram = void . ram (always 0) . mealy step 0
  where
    step i Nothing = (i, Nothing)
    step i (Just item) = (succ i, Just (i, item))
