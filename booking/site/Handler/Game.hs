module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

getGameR :: GameId -> Handler Html
getGameR gameId = do
    Just game <- runDB $ do
        get gameId
    timeslots <- runDB $ do
        selectList [TimeslotGame ==. gameId] []

    (formWidget, formEnctype) <- generateFormPost sampleForm
    defaultLayout $ do
        setTitle "Locked Danang"
        $(widgetFile "game")

postBookingR :: Handler Html
postBookingR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
