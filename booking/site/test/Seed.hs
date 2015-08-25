{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import Data.Time.LocalTime

insertGame :: MonadIO m => (Text, Text) -> ReaderT SqlBackend m ()
insertGame (name, description) =
    insert_ $ Game name description 

insertTimeslot :: MonadIO m => GameId -> (Day, TimeOfDay) -> ReaderT SqlBackend m ()
insertTimeslot gameId (day, time) =
    insert_ $ Timeslot day time gameId

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
      runMigration migrateAll
      --deleteWhere ([] :: [Filter Game])
      --mapM_ insertGame games
      let Right gameKey = keyFromValues [PersistInt64 1]
      mapM_ (insertTimeslot gameKey) $ timeslots (fromGregorian 2015 8 23) 3

