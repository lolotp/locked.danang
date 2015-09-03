{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Booking where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3
                              , withSmallInput)
import Yesod.Form.Jquery    (YesodJquery(..))

import Data.Time.LocalTime
import qualified Data.Text as T

import PNotify               (setPNotify, PNotify(..), NotifyType(..), NotifyStyling(..))
import Handler.Game (bookingForm)


getBookingsR :: GameId -> Handler Html
getBookingsR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    ((formResult, formWidget), formEnctype) <- runFormGet $ timeDurationForm $ localDay currentTime
    let (queryDay, queryNumOfDay) = case formResult of { FormSuccess pair -> pair ; _ -> (localDay currentTime, 1)}
    bookedTimeslots <- runDB $ gameBookingsQuery gameId queryDay queryNumOfDay

    defaultLayout $ do
        master <- getYesod
        addScriptEither $ urlJqueryJs master
        setTitle "Bookings"
        $(widgetFile "bookings")

postDeleteBookingR :: BookingId -> Handler TypedContent
postDeleteBookingR bookingId = do
    (mbGame, _) <- runDB $ findAndDeleteBookingQuery bookingId
    case mbGame of
        Just gameId -> redirect $ BookingsR gameId
        Nothing -> redirect HomeR

deleteBookingR :: BookingId -> Handler TypedContent
deleteBookingR bookingId = do
    (_, nDelete) <- runDB $ findAndDeleteBookingQuery bookingId
    selectRep $ do
        provideRep $ return $ object
            [ "number_of_rows_deleted" .= nDelete]

postBookingsR :: GameId -> Handler Html
postBookingsR gameId = do
    ((res, _), _) <- runFormPost $ bookingForm
    newBookingError <- case res of
        FormSuccess booking -> 
            runDB $ do
                insertRes <- insertBy booking
                case insertRes of
                    Left _ -> return $ Just "Timeslot has already been booked!"
                    Right _ -> return Nothing
            `catch` (\ (e :: SomeException)  -> do
                $(logError) $ T.pack $ show e
                return $ Just "Database error, please try again later"
            )
        FormFailure msg -> return $ Just $ T.concat msg
        FormMissing -> return $ Just "FormMissing"
    case newBookingError of
        Just msg -> do
            $(logError) msg
            -- getI18NMessage <- getMessageRender
            -- setMessage $ toHtml $  getI18NMessage MsgInvalidBookingData
            setPNotify $ PNotify JqueryUI Error "Error" "Fail to add new booking"
        Nothing -> return ()
    redirect (GameR gameId)

timeDurationForm :: Day -> Form (Day, Integer)
timeDurationForm defaultDay =
    renderBootstrap3 BootstrapBasicForm $ (,)
        <$> areq dayField (withSmallInput "Day") (Just defaultDay)
        <*> areq intField (withSmallInput "Number of day") (Just 1) 
