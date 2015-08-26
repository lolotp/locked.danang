module Handler.Game where

import Import
import Data.FileEmbed (embedFile)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

vietnamTimezone :: TimeZone
vietnamTimezone = TimeZone 420 False ""

vietnamCurrentTime :: IO LocalTime
vietnamCurrentTime =
    getCurrentTime >>= return . (utcToLocalTime vietnamTimezone)

weekDateName :: Day -> Text
weekDateName day =
    let (_, _, weekDate) = toWeekDate day in
    case weekDate of
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        7 -> "Sunday"
        _ -> ""

getGameR :: GameId -> Handler Html
getGameR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    Just game <- runDB $ do
        get gameId
    let timeslots = timeslotsFromDay (localDay currentTime) 3
    let days = map (\num -> addDays num (localDay currentTime)) [0..20]

    defaultLayout $ do
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")

postBookingR :: Handler TypedContent
postBookingR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

