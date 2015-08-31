{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              BootstrapGridOptions (..), withSmallInput)

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Maybe (fromJust)

import qualified Data.Text as T
import qualified Data.Map  as DM

vietnamTimezone :: TimeZone
vietnamTimezone = TimeZone 420 False ""

vietnamCurrentTime :: IO LocalTime
vietnamCurrentTime =
    getCurrentTime >>= return . (utcToLocalTime vietnamTimezone)

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

getGameR :: GameId -> Handler Html
getGameR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    (game, timeslots) <- runDB $ do
        game <- get404 gameId
        timeslots <- availableGameTimeslotsBookingQuery gameId currentTime
        return (game, timeslotBookingMapFromList timeslots)
    let days = map (\num -> addDays num (localDay currentTime)) [0..numDaysInAdvance-1]
    (formWidget, formEnctype) <- generateFormPost bookingForm

    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")


getBookingsR :: GameId -> Handler Html
getBookingsR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    ((formResult, formWidget), formEnctype) <- runFormGet $ timeDurationForm $ localDay currentTime
    let (queryDay, queryNumOfDay) = case formResult of { FormSuccess pair -> pair ; _ -> (localDay currentTime, 1)}
    bookedTimeslots <- runDB $ do
        queriedSlots <- gameBookingsQuery gameId queryDay queryNumOfDay
        return [ (ts, b) | (Entity _ ts, Entity _ b) <- queriedSlots]

    defaultLayout $ do
        setTitle "Bookings"
        $(widgetFile "bookings")

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
        Just msg -> $(logError) msg
        Nothing -> return ()
    redirect (GameR gameId)

addAttr :: Text -> Text -> [(Text,Text)] -> [(Text,Text)]
addAttr attrName attr []                      = [(attrName, attr)]
addAttr attrName attr ((name, oldValue):rest) = 
    if (name ==attrName) then
        (attrName, T.concat [oldValue, " ", attr]) : rest
    else
        (name, oldValue) : addAttr attrName attr rest

customFieldSettings :: FieldSettings site -> FieldSettings site
customFieldSettings fs =
    let smallInputFs = withSmallInput fs in
    smallInputFs { fsAttrs = addClass "form-control" (fsAttrs smallInputFs) }
    where
        addClass = addAttr "class"

rangeFieldSettings :: Int -> Int -> FieldSettings site -> FieldSettings site
rangeFieldSettings minValue maxValue fs = 
    let attrsWithMin = addAttr "min-disabled" (T.pack (show minValue)) (fsAttrs fs) in
    let finalAttrs = addAttr "max-disabled" (T.pack (show maxValue)) attrsWithMin in
    fs { fsAttrs = finalAttrs}

bookingForm :: Form Booking
bookingForm = 
    let formLayout = BootstrapHorizontalForm (ColLg 0) (ColLg 3) (ColLg 0) (ColLg 9) in
    renderBootstrap3 formLayout $ Booking
        <$> areq textField (customFieldSettings (fieldSettingsLabel MsgName)) Nothing
        <*> areq emailField (customFieldSettings "Email") Nothing
        <*> areq intField (customFieldSettings "Phone") Nothing
        <*> areq nPeopleField (rangeFieldSettings 1 20 (customFieldSettings "Number of People")) Nothing
        <*> areq hiddenField "" Nothing
    where
        nPeopleField = checkBool (\n -> n>0 && n <= 20) ("There are too few or too many people!!" :: Text) intField

timeDurationForm :: Day -> Form (Day, Integer)
timeDurationForm defaultDay =
    renderBootstrap3 BootstrapBasicForm $ (,)
        <$> areq dayField (withSmallInput "Day") (Just defaultDay)
        <*> areq intField (withSmallInput "Number of day") (Just 1)

