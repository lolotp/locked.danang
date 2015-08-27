{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import Database.Persist.Sql (runMigrationUnsafe)
import Data.Time.LocalTime

insertGame :: MonadIO m => (Text, Text) -> ReaderT SqlBackend m ()
insertGame (name, description) =
    insert_ $ Game name description 

games :: [(Text, Text)]
games =
    [ ("Saw",     "Lưỡi cưa tử thần")
    , ("Pharaohs", "Lăng mộ cổ")
    ]

main :: IO ()
main = do
    settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
    let conn = (pgConnStr $ appDatabaseConf settings)
    runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
        runMigrationUnsafe migrateAll
        deleteWhere ([] :: [Filter Game])
        mapM_ insertGame games
        games <- selectList ([] :: [Filter Game]) []
        mapM_ 
            (\game -> mapM_ (insertTimeslot (entityKey game)) $ timeslotsFromDay (fromGregorian 2015 8 23) 3) 
            games

