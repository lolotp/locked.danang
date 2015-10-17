{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Model
import Control.Monad (zipWithM_)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import Database.Persist.Sql (runMigrationUnsafe)
import Data.Time.LocalTime

insertGameAndImages :: MonadIO m => Game -> [Text] -> ReaderT SqlBackend m () 
insertGameAndImages game urlList = do
    gameId <- insert game
    mapM_ (\url -> insert_ (PreviewImage url gameId)) urlList

pharaohDescription :: Text
pharaohDescription = "Cái chết nhanh chóng đến với kẻ nào dám xâm phạm sự thanh bình của hoàng đế. Lời nguyền chết chóc của vui Tutankhamun vẫn là một bí ẩn và thử thách hấp dẫn với các nhà thám hiểm trẻ. Tuy nhiên, một khi đã đặt chân vào lăng mộ linh thiêng, chỉ có hai con đường: Phá lời nguyền và thoát ra hoặc nhận lấy sự trừng phạt của thần chết."

sawDescription :: Text
sawDescription = "Kẻ giết người hàng loạt JigSaw đã trở lại cùng với những cạm bẫy tra tấn khủng khiếp. Liệu rằng các nạn nhân tiếp theo có thể thoát khỏi sự độc ác của hắn."

inceptionDescription :: Text
inceptionDescription = "Một ngày nọ, thời gian tạo ra một cái bẫy. Bạn và các bạn của mình phát hiện ra tất cả đang bị khóa trong một mật thất không lối thoát. Tất cả phải cố gắng để cùng nhau thoát ra khỏi không gian vừa ảo vừa thật này. Hi vọng thoát thân nằm ở việc nắm giữ được không gian và thời gian. Nếu không, bạn và mọi người sẽ mãi bị giam cầm nơi đây…."

games :: [Game]
games =
    [ Game "Saw"      sawDescription 100 4 8 "http://dn.locked.vn/sites/default/files/styles/medium/public/image/thumb_saw.png"
    , Game "Pharaohs" pharaohDescription  80 4 8 "http://dn.locked.vn/sites/default/files/styles/medium/public/image/thumb_pharaohs.png"
    , Game "Inception" inceptionDescription 70 3 6 "http://dn.locked.vn/files/inception/thumb.jpg"
    ]

gamePreviewImages :: [[Text]]
gamePreviewImages = [ ["http://dn.locked.vn/sites/default/files/styles/image_slide/public/image/saw_1.jpg","http://dn.locked.vn/sites/default/files/styles/image_slide/public/image/saw_2.jpg"]
                    , ["http://dn.locked.vn/sites/default/files/styles/image_slide/public/image/pha_7.jpg","http://hcm.locked.vn/sites/default/files/styles/image_slide/public/image/pha_8.jpg"]
                    , ["http://dn.locked.vn/files/inception/slide_0.jpg","http://dn.locked.vn/files/inception/slide_1.jpg","http://dn.locked.vn/files/inception/slide_2.jpg"]
                    ]

main :: IO ()
main = do
    currentTime <- getCurrentTime >>= return . (utcToLocalTime (TimeZone 420 False ""))

    settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
    let conn = (pgConnStr $ appDatabaseConf settings)
    runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
        -- deleteWhere ([] :: [Filter Booking])
        -- deleteWhere ([] :: [Filter Timeslot])
        -- deleteWhere ([] :: [Filter PreviewImage])
        -- deleteWhere ([] :: [Filter Game])
        runMigrationUnsafe migrateAll
        zipWithM_ insertGameAndImages games gamePreviewImages
        games <- selectList ([] :: [Filter Game]) []
        mapM_
            (\game -> mapM_ (insertTimeslot (entityKey game)) $ timeslotsFromDay (fromGregorian 2015 10 17) 52)
            games

