{-# LANGUAGE RecordWildCards          #-}

module Database
    ( createTables
    , insertUserInfo
    , getUserInfo) where

import Types
import Database.HDBC (run, commit, toSql, execute, prepare, quickQuery', disconnect, SqlValue, fromSql)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text)



createTables :: IO ()
createTables = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    _ <- run conn "CREATE TABLE UserInfo (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, name VARCHAR(40) NOT NULL, phone VARCHAR(40), email VARCHAR(80) NOT NULL, message VARCHAR(255) NOT NULL)" []  
    commit conn

insertUserInfo :: UserInformation -> IO () 
insertUserInfo UserInformation{..} = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    stmt <- prepare conn "INSERT OR IGNORE INTO UserInfo (name, phone, email, message) VALUES (?,?,?,?)" 
    let sqlValues = map toSql [userName, fromMaybe (pack "") userPhone, fromMaybe (pack "") userEmail, userMessage] 
    _ <- execute stmt sqlValues
    commit conn

getUserInfo :: IO UserInformationList
getUserInfo = do 
    conn <- connectSqlite3 "../../../db/UserInfo.db"
    query <- quickQuery' conn "SELECT * FROM UserInfo" []   -- :t query = [[SqlValue]]
    let userInfoList = map transferValue query
    disconnect conn
    return $ UserInformationList userInfoList

transferValue :: [SqlValue] -> UserInformation
transferValue s =  
    let textList = map f s 
    in UserInformation (textList!!1) (Just (textList!!2)) (Just (textList!!3)) (textList!!4)
  where
    f :: SqlValue -> Text
    f = fromSql 

-- sampleUI :: UserInformation
-- sampleUI = UserInformation  (pack "Tinsley") Nothing (pack "123@123.com") (pack "hello world")