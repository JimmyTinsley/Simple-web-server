{-# LANGUAGE RecordWildCards          #-}

module Database
    ( createTables
    , insertUserInfo
    , getUserInfo
    ) where

-- Database module need Types module.
import Types
-- All functions used in this module are listed.
import Database.HDBC (run, commit, toSql, execute, prepare, quickQuery', disconnect, SqlValue, fromSql)
import Database.HDBC.Sqlite3 (connectSqlite3) -- We use Sqlite3 as our database end.
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text)
import Data.Time.Clock (UTCTime)

-- Function used to create the table we need.
createTables :: IO ()
createTables = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    _ <- run conn "CREATE TABLE UserInfo (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                                         \name VARCHAR(40) NOT NULL, phone VARCHAR(40), \
                                         \email VARCHAR(80) NOT NULL, message VARCHAR(255) NOT NULL, \
                                         \time TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL)" []  
                                         -- Sqlite3 will automatically add current timestamp into database
                                         -- when inserting data since we have set DEFAULT CURRENT_TIMESTAMP 
                                         -- for time column.
    commit conn

-- Function used to insert data converted from post request to database.
-- It doesn't insert timestamp itself, Sqlite will do it. 
insertUserInfo :: UserInformation -> IO () 
insertUserInfo UserInformation{..} = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    stmt <- prepare conn "INSERT OR IGNORE INTO UserInfo (name, phone, email, message) VALUES (?,?,?,?)" 
    -- If userPhone or userEmail is empty, empty value will be inserted. 
    let sqlValues = map toSql [userName, fromMaybe (pack "") userPhone, fromMaybe (pack "") userEmail, userMessage]
    _ <- execute stmt sqlValues
    commit conn

-- Function used to get data from database and save them in UserInformationList type. 
getUserInfo :: IO UserInformationList
getUserInfo = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    query <- quickQuery' conn "SELECT * FROM UserInfo" []   -- :t query = [[SqlValue]]
    let userInfoList = map transformValue query
    disconnect conn
    return $ UserInformationList userInfoList

-- Function used to do some important data type transformation
transformValue :: [SqlValue] -> UserInformation
transformValue s =  
    let 
        usrName    = fromSql (s!!1) :: Text
        usrPhone   = fromSql (s!!2) :: Text
        usrEmail   = fromSql (s!!3) :: Text
        usrMessage = fromSql (s!!4) :: Text
        msgTime    = fromSql (s!!5) :: UTCTime
    in UserInformation usrName (Just usrPhone) (Just usrEmail) usrMessage (Just msgTime)
    -- Use "let ... in ..." instead of "do ... where ..." because there is not IO in this function
