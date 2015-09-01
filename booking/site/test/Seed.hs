{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import Database.Persist.Sql (runMigrationUnsafe)
import Data.Time.LocalTime

insertGame :: MonadIO m => (Text, Text, Int, Int, Int, Text) -> ReaderT SqlBackend m () 
insertGame (name, description, difficulty, minPeople, maxPeople, imageUrl) =
    insert_ $ Game name description difficulty minPeople maxPeople imageUrl

games :: [(Text, Text, Int, Int, Int, Text)]
games =
    [ ("Saw",     "Lưỡi cưa tử thần", 100, 6, 8, "http://dn.locked.vn/sites/default/files/styles/medium/public/image/thumb_saw.png?itok=eVQZnFqE")
    , ("Pharaohs", "Lăng mộ cổ", 80, 8, 10, "http://dn.locked.vn/sites/default/files/styles/medium/public/image/thumb_pharaohs.png?itok=CSjtoyUN")
    ]

main :: IO ()
main = do
    settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
    let conn = (pgConnStr $ appDatabaseConf settings)
    runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
        -- deleteWhere ([] :: [Filter Booking])
        -- deleteWhere ([] :: [Filter Timeslot])
        -- deleteWhere ([] :: [Filter Game])
        runMigrationUnsafe migrateAll
        mapM_ insertGame games
        games <- selectList ([] :: [Filter Game]) []
        mapM_ 
            (\game -> mapM_ (insertTimeslot (entityKey game)) $ timeslotsFromDay (fromGregorian 2015 8 23) 3) 
            games

