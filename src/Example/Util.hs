module Example.Util (always,Blockram) where

import Clash.Prelude
import Data.Function

type Blockram dom n a = Signal dom (Index n) -> Signal dom (Maybe (Index n, a)) -> Signal dom a

always :: (HiddenClockResetEnable dom, NFDataX a) => a -> Signal dom a
always x = fix (register x)

