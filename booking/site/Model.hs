module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Map.Strict (Map)
import Data.Map (fromList)
import Data.List (head)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.),(?.))
import Database.Persist.Sql (deleteWhereCount)

import Control.Monad.Trans.Maybe

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


---------------------------------------------
-- Application data constants
---------------------------------------------

timeslotsPerDay :: Day -> [TimeOfDay]
timeslotsPerDay day =
    let (_, _, weekDate) = toWeekDate day in
    let f h m = TimeOfDay h m 0 in
    let weekDaySlots = [f 08 15, f 09 30, f 10 45, f 13 30, f 14 45, f 16 00, f 17 15, f 20 45, f 18 30, f 19 45] in
    let weekEndSlots = [f 08 15, f 09 30, f 10 45, f 12 00, f 13 30, f 14 45, f 16 00, f 17 15, f 20 45, f 18 30, f 19 45, f 21 00] in
    case weekDate of
        6 -> weekEndSlots
        7 -> weekEndSlots
        _ -> weekDaySlots

timeslotsFromDay :: Day -> Int -> [(Day, TimeOfDay)]
timeslotsFromDay startDate numWeeks =
  let dayList = map (\numDays -> addDays (toInteger numDays) startDate) [0..numWeeks*7-1] in
  [(day, time) | day <- dayList, time <- timeslotsPerDay day]
  -- [(day, time) | day <- dayList, time <- timeslotsPerDay]

insertTimeslot :: MonadIO m => GameId -> (Day, TimeOfDay) -> ReaderT SqlBackend m ()
insertTimeslot gameId (day, time) =
    insert_ $ Timeslot day time gameId

numWeeksInAdvance :: Integer
numWeeksInAdvance = 3

numDaysInAdvance :: Integer
numDaysInAdvance = numWeeksInAdvance * 7

---------------------------------------------
-- Database queries
---------------------------------------------

-- | Type to describe joined timeslot Booking data
type TimeslotBooking = (E.Value Day, E.Value TimeOfDay, E.Value TimeslotId, E.Value (Maybe BookingId))

-- | Query to find all available timeslots and their associated bookings
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
    E.limit $ fromIntegral $ numDaysInAdvance * (toInteger.length) (timeslotsPerDay (fromGregorian 2015 10 17))
    return
        ( timeslot ^. TimeslotDay
        , timeslot ^. TimeslotTime
        , timeslot ^. TimeslotId
        , booking  ?. BookingId
        )

gameBookingsQuery :: MonadIO m => GameId -> Day -> Integer -> E.SqlPersistT m [(Entity Timeslot, Entity Booking)]
gameBookingsQuery gameId fromDay numDay = E.select
    $ E.from $ \ (booking `E.InnerJoin` timeslot) -> do
    E.on $ booking ^. BookingTimeslot E.==. timeslot ^. TimeslotId
    E.where_ $ 
        (timeslot ^. TimeslotGame E.==. E.val gameId)
        E.&&. (timeslot ^. TimeslotDay E.>=. E.val fromDay )
        E.&&. (timeslot ^. TimeslotDay E.<. E.val (addDays numDay fromDay) )
    return (timeslot, booking)

findAndDeleteBookingQuery :: MonadIO m => BookingId -> ReaderT SqlBackend m (Maybe (Key Game), Int64)
findAndDeleteBookingQuery bookingId = do
    mbGame <-  runMaybeT $ do
        booking <- MaybeT $ get bookingId
        ts <- MaybeT $ get (bookingTimeslot booking)
        return (timeslotGame ts)
    nDelete <- case mbGame of
        Nothing -> return 0
        Just _ -> deleteWhereCount [BookingId ==. bookingId]
    return (mbGame, nDelete)

timeslotBookingMapFromList :: [TimeslotBooking] -> Data.Map.Strict.Map (Day,TimeOfDay) (TimeslotId, (Maybe BookingId))
timeslotBookingMapFromList list =
    Data.Map.fromList
        [((day, time), (timeslotId, bookingId)) | (E.Value day,E.Value time,E.Value timeslotId, E.Value bookingId) <- list]

keyToInt64 :: PersistEntity a => Key a -> Int64
keyToInt64 key =
    let PersistInt64 entityId = Data.List.head $ keyToValues key in
    entityId

