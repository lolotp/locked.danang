{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              BootstrapGridOptions (..), withSmallInput)

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import qualified Data.Text as T
import qualified Data.Map  as DM
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.),(?.))

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

numWeeksInAdvance :: Integer
numWeeksInAdvance = 3

numDaysInAdvance :: Integer
numDaysInAdvance = numWeeksInAdvance * 7

getGameR :: GameId -> Handler Html
getGameR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    (game, timeslots) <- runDB $ do
        game <- get404 gameId

        -- perform SQL join with Esqueleto EDSL language
        timeslots <- E.select 
            $ E.from $ \ (timeslot `E.LeftOuterJoin` booking) -> do
                E.on $ E.just (timeslot ^. TimeslotId) E.==. booking ?. BookingTimeslot
                E.where_ $ (timeslot ^. TimeslotGame E.==. E.val gameId)
                     E.&&. (timeslot ^. TimeslotDay  E.>=. E.val (localDay currentTime))
                     E.&&. (timeslot ^. TimeslotTime E.>.  E.val (localTimeOfDay currentTime))
                E.limit $ fromIntegral $ numDaysInAdvance * (toInteger.length) timeslotsPerDay
                return
                    ( timeslot ^. TimeslotDay
                    , timeslot ^. TimeslotTime
                    , booking  ?. BookingId
                    )
        return (game, DM.fromList [((day, time), bookingId) | (E.Value day,E.Value time,E.Value bookingId) <- timeslots])
    $(logInfo) $ T.pack $ show timeslots
    let days = map (\num -> addDays num (localDay currentTime)) [0..numDaysInAdvance-1]
    (formWidget, formEnctype) <- generateFormPost bookingForm

    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")


postBookingR :: GameId -> Handler Html
postBookingR gameId = do
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
        <$> areq textField (customFieldSettings "Name") Nothing
        <*> areq emailField (customFieldSettings "Email") Nothing
        <*> areq intField (customFieldSettings "Phone") Nothing
        <*> areq nPeopleField (rangeFieldSettings 1 20 (customFieldSettings "Number of People")) Nothing
        <*> areq hiddenField "" Nothing
    where
        nPeopleField = checkBool (\n -> n>0 && n <= 20) ("There are too few or too many people!!" :: Text) intField
        
