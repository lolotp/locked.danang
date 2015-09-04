{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              BootstrapGridOptions (..), withSmallInput)
import Yesod.Form.Jquery     (YesodJquery(..))

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Maybe (fromJust)

import qualified Data.Text as T
import qualified Data.Map  as DM

import Text.Julius (rawJS)

getGameR :: GameId -> Handler Html
getGameR gameId = do
    currentTime <- liftIO vietnamCurrentTime
    game <- runDB $ get404 gameId
    timeslots  <- runDB $ do
        ts <- availableGameTimeslotsBookingQuery gameId currentTime
        return $ timeslotBookingMapFromList ts
    previewImages <- runDB $ selectList [PreviewImageGame ==. gameId] []
    let days = map (\num -> addDays num (localDay currentTime)) [0..numDaysInAdvance-1]
    (formWidget, formEnctype) <- generateFormPost bookingForm

    defaultLayout $ do
        master <- getYesod
        addScriptEither $ urlJqueryJs master
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        setTitle $ toHtml $ gameName game
        $(widgetFile "game")

---------------------------------------------
-- Form & form helpers
---------------------------------------------

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

bookingFormTimeslotFieldId :: Text
bookingFormTimeslotFieldId = "booking_timeslot_id"

bookingForm :: Form Booking
bookingForm =
    let formLayout = BootstrapHorizontalForm (ColLg 0) (ColLg 3) (ColLg 0) (ColLg 9) in
    renderBootstrap3 formLayout $ Booking
        <$> areq textField (customFieldSettings (fieldSettingsLabel MsgName)) Nothing
        <*> areq emailField (customFieldSettings (fieldSettingsLabel MsgEmail)) Nothing
        <*> areq intField (customFieldSettings (fieldSettingsLabel MsgPhone)) Nothing
        <*> areq nPeopleField (rangeFieldSettings 1 20 (customFieldSettings (fieldSettingsLabel MsgNumberOfPeople))) Nothing
        <*> areq hiddenField (FieldSettings "" Nothing (Just bookingFormTimeslotFieldId) Nothing []) Nothing
    where
        nPeopleField = checkBool (\n -> n>0 && n <= 20) ("There are too few or too many people!!" :: Text) intField

