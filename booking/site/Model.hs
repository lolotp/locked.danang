module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Time.LocalTime
import Data.Time.Calendar

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

timeslotsPerDay :: [TimeOfDay]
timeslotsPerDay = map (\hour -> TimeOfDay hour 0 0) [9..21]

timeslots :: Day -> Int -> [(Day, TimeOfDay)]
timeslots startDate numWeeks = 
  let dayList = map (\numDays -> addDays (toInteger numDays) startDate) [0..numWeeks*7-1] in
  [(day, time) | day <- dayList, time <- timeslotsPerDay]

insertTimeslot :: MonadIO m => GameId -> (Day, TimeOfDay) -> ReaderT SqlBackend m ()
insertTimeslot gameId (day, time) =
    insert_ $ Timeslot day time gameId

