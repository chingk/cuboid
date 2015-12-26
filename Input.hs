{-# LANGUAGE Arrows #-}
module Input (parseInput) where

import FRP.Yampa
import FRP.Yampa.Utilities

import Graphics.UI.GLUT

import Types

-- Event Definition:
filterKeyDowns :: SF (Event Input) (Event Input) 
filterKeyDowns = arr $ filterE ((==Down) . keyState) --上下左右鍵，球會跑

keyIntegral :: Double -> SF (Event a) Double
keyIntegral a = let eventToSpeed (Event _) = a
                    eventToSpeed NoEvent   = 0 
                in arr eventToSpeed >>> integral  --arr update to a SF(signal function)
                       
-- Input
parseInput :: SF (Event Input) ParsedInput --press which button
parseInput = proc i -> do
    down     <- filterKeyDowns                  -< i
    wCount   <- countKey 'w'                    -< down
    aCount   <- countKey 'a'                    -< down
    sCount   <- countKey 's'                    -< down
    dCount   <- countKey 'd'                    -< down
    upEvs    <- filterKey (SpecialKey KeyUp)    -< down --event看還是累計的來看
    downEvs  <- filterKey (SpecialKey KeyDown)  -< down
    rightEvs <- filterKey (SpecialKey KeyRight) -< down
    leftEvs  <- filterKey (SpecialKey KeyLeft)  -< down
    periodEvs <- filterKey (Char '.')    -< down
    commaEvs  <- filterKey (Char ',')    -< down
    returnA -< ParsedInput wCount aCount sCount dCount 
                           upEvs downEvs rightEvs leftEvs periodEvs commaEvs ---把8個func串起來parse出去
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 1 --取出來的key跟character的值是否一樣
          filterKey k = arr $ filterE ((==k) . key)


