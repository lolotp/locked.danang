module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

---------------------------------------------
-- Time related helpers
---------------------------------------------

-- | Timezone of Vietnam
vietnamTimezone :: TimeZone
vietnamTimezone = TimeZone 420 False ""

-- | Get current local time in Vietnam based on UTC
vietnamCurrentTime :: IO LocalTime
vietnamCurrentTime =
    getCurrentTime >>= return . (utcToLocalTime vietnamTimezone)

-- | Map days in the week to their respective names based on their ordering
weekDateName :: Day -> Text
weekDateName day =
    let (_, _, weekDate) = toWeekDate day in
    case weekDate of
        1 -> "Mon"
        2 -> "Tue"
        3 -> "Wed"
        4 -> "Thu"
        5 -> "Fri"
        6 -> "Sat"
        7 -> "Sun"
        _ -> ""

