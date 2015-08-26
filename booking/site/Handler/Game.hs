module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.FileEmbed (embedFile)
import Data.Time.LocalTime

vietnamTimezone :: TimeZone
vietnamTimezone = TimeZone 420 False ""

vietnamCurrentTime :: IO LocalTime
vietnamCurrentTime =
    getCurrentTime >>= return . (utcToLocalTime vietnamTimezone)

getGameR :: GameId -> Handler Html
getGameR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    Just game <- runDB $ do
        get gameId
    timeslots <- runDB $ do
        selectList [TimeslotGame ==. gameId, TimeslotDay >=. localDay currentTime] []

    defaultLayout $ do
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")

postBookingR :: Handler TypedContent
postBookingR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

