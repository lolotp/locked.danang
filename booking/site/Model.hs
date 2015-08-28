module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Map.Strict (Map)
import Data.Map (fromList)
import Data.List (head)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.),(?.))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

timeslotsPerDay :: [TimeOfDay]
timeslotsPerDay = map (\hour -> TimeOfDay hour 0 0) [9..21]

timeslotsFromDay :: Day -> Int -> [(Day, TimeOfDay)]
timeslotsFromDay startDate numWeeks = 
  let dayList = map (\numDays -> addDays (toInteger numDays) startDate) [0..numWeeks*7-1] in
  [(day, time) | day <- dayList, time <- timeslotsPerDay]

insertTimeslot :: MonadIO m => GameId -> (Day, TimeOfDay) -> ReaderT SqlBackend m ()
insertTimeslot gameId (day, time) =
    insert_ $ Timeslot day time gameId

numWeeksInAdvance :: Integer
numWeeksInAdvance = 3

numDaysInAdvance :: Integer
numDaysInAdvance = numWeeksInAdvance * 7

-- type to describe joined timeslot Booking data
type TimeslotBooking = (E.Value Day, E.Value TimeOfDay, E.Value TimeslotId, E.Value (Maybe BookingId))

-- query to find all available timeslots and their associated bookings
-- the implementation is SQL join with Esqueleto EDSL language
availableGameTimeslotsBookingQuery :: MonadIO m => GameId -> LocalTime -> E.SqlPersistT m [TimeslotBooking]
availableGameTimeslotsBookingQuery gameId currentTime = E.select 
    $ E.from $ \ (timeslot `E.LeftOuterJoin` booking) -> do
    E.on $ E.just (timeslot ^. TimeslotId) E.==. booking ?. BookingTimeslot
    E.where_ $ 
        (timeslot ^. TimeslotGame E.==. E.val gameId)
        E.&&.(
                  (timeslot ^. TimeslotDay  E.>. E.val (localDay currentTime))
             E.||.(
                       (timeslot ^. TimeslotDay E.==. E.val (localDay currentTime))
                  E.&&.(timeslot ^. TimeslotTime E.>.  E.val (localTimeOfDay currentTime))
                  )
             )
    E.limit $ fromIntegral $ numDaysInAdvance * (toInteger.length) timeslotsPerDay
    return
        ( timeslot ^. TimeslotDay
        , timeslot ^. TimeslotTime
        , timeslot ^. TimeslotId
        , booking  ?. BookingId
        )

timeslotBookingMapFromList :: [TimeslotBooking] -> Data.Map.Strict.Map (Day,TimeOfDay) (TimeslotId, (Maybe BookingId))
timeslotBookingMapFromList list = 
    Data.Map.fromList
        [((day, time), (timeslotId, bookingId)) | (E.Value day,E.Value time,E.Value timeslotId, E.Value bookingId) <- list]

keyToInt64 :: PersistEntity a => Key a -> Int64
keyToInt64 key =
    let PersistInt64 entityId = Data.List.head $ keyToValues key in
    entityId

