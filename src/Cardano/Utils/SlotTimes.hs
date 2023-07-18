module Cardano.Utils.SlotTimes
  ( slotStart
  , systemStart
  ) 
where

-- base:
import           Data.Time

-- cardano packages:
import           Cardano.Slotting.Slot


slotStart :: SlotNo -> UTCTime
slotStart =
    flip addUTCTime timeWhenSlotChangedTo1Sec
  . (* slotLength)
  . (\s-> s - (fromIntegral $ unSlotNo slotWhenSlotChangedTo1Sec))
  . fromIntegral
  . unSlotNo


-- FIXME[R2]: is there code to achieve the same function
--  in the cardano libraries.  Simple to use (here we have simplifying
--  preconditions.

slotWhenSlotChangedTo1Sec :: SlotNo
slotWhenSlotChangedTo1Sec = SlotNo 4492800

timeWhenSlotChangedTo1Sec :: UTCTime
timeWhenSlotChangedTo1Sec = convertTime $ "2020-07-29T21:44:51Z"
 -- convertTime $ "2020-07-28T20:20:16Z"
 -- POSIXTime 1595967616000  -- 2020/07/28 20:20:16 - epoch:74 - slot:1598400 - block:1597133  


systemStart :: UTCTime
systemStart = convertTime $ "2017-09-23T21:44:51Z"


slotLength :: NominalDiffTime  -- '... will treat it as seconds'
slotLength  = 1

-- | slotStart slot - UTCTime when slot starts
--
-- Preconditions: 
--   slot >= slotWhenSlotChangedTo1Sec


---- utilities -----------------------------------------------------

convertTime :: String -> UTCTime
convertTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

