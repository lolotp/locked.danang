module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              BootstrapGridOptions (..), withSmallInput)

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import qualified Data.Text as T

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
    game <- runDB $ do
        get404 gameId
    let days = map (\num -> addDays num (localDay currentTime)) [0..20]
    (formWidget, formEnctype) <- generateFormPost bookingForm

    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")


postBookingR :: GameId -> Handler Html
postBookingR gameId = do
    ((res, _), _) <- runFormPost $ bookingForm
    $(logInfo) $ T.pack $ show res
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
    let attrsWithMin = addAttr "min" (T.pack (show minValue)) (fsAttrs fs) in
    let finalAttrs = addAttr "max" (T.pack (show maxValue)) attrsWithMin in
    fs { fsAttrs = finalAttrs}

bookingForm :: Form Booking
bookingForm = 
    let formLayout = BootstrapHorizontalForm (ColLg 0) (ColLg 3) (ColLg 0) (ColLg 9) in
    renderBootstrap3 formLayout $ Booking
        <$> areq textField (customFieldSettings "Name") Nothing
        <*> areq emailField (customFieldSettings "Email") Nothing
        <*> areq intField (customFieldSettings "Phone") Nothing
        <*> areq nPeopleField (rangeFieldSettings 1 20 (customFieldSettings "Number of People")) Nothing
        <*> areq dayField (customFieldSettings "Day") Nothing
        <*> areq timeFieldTypeText (customFieldSettings "Time") Nothing
    where
        nPeopleField = checkBool (>0) ("Must exceed 0" :: Text) intField
        

