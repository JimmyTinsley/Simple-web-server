{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE QuasiQuotes       #-}

module Main 
    ( main
    )where

-- Other two modules of the program
import Types 
import Database

-- Modules needed
import Control.Monad
import Data.Aeson
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Directory (findFile)
import Data.Maybe (isNothing)

import qualified Data.ByteString.Lazy.Char8 as S8

main :: IO ()
main = do
    args <- getArgs
    let port = (read $ head args) :: Int 
    databaseDir <- findFile ["../../../db"] "UserInfo.db"
    when (isNothing databaseDir) createTables
    putStrLn "Server Started."
    run port app
    

app :: Application
app req send = 
    
    case pathInfo req of 
        ["get"]  -> if requestMethod req == methodGet
                    then handleGetRequest req send
                    else send $ responseLBS
                        status405 
                        [("Content-Type", "text/plain; charset=utf-8")]
                        "Mehtod Not Allowed"

        ["post"] -> if requestMethod req == methodPost 
                    then handlePostRequest req send 
                    else send $ responseLBS
                        status405 
                        [("Content-Type", "text/plain; charset=utf-8")]
                        "Mehtod Not Allowed"

        _        -> send $ responseLBS
            status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "Not found"


handleGetRequest :: Application
handleGetRequest _ send = do 
    usrInfoList <- getUserInfo
    let responseByteString = encode1 usrInfoList
    putStrLn $ S8.unpack responseByteString

    send $ responseLBS 
            status200
            [("Content-Type", "application/json; charset=utf-8"),("Access-Control-Allow-Origin","*")]
            responseByteString
  where
    encode1 :: UserInformationList -> S8.ByteString
    encode1 = encode



handlePostRequest :: Application 
handlePostRequest req send = do
    -- Get the body of the request and store it in ByteString
    reqBdy <- lazyRequestBody req 

    -- Show the  requst body in log
    putStrLn $ S8.unpack reqBdy

    -- For some unknown reasons(maybe something about charset encode?), we need to pack and
    -- unpack the ByteString, and then pass it to decode function to make it work normally
    let requestByteString = S8.pack $ S8.unpack reqBdy 

    -- The input and out types of decode function need to set, write it as decode1
    let usrInfo = decode1 requestByteString 
    
    -- If decode success, then return 200 OK, else return 400 Bad Request
    case usrInfo of 
        Nothing -> do 
            putStrLn "usrInfo1 Nothing"
            send $ responseLBS
                status400
                [("Content-Type", "text/plain; charset=utf-8"), ("Access-Control-Allow-Origin","*")]
                "Bad Request"
        Just ui -> do
            -- Print the UserInformation for debugging
            print ui
            insertUserInfo ui
            send $ responseLBS
                status200
                [("Content-Type", "text/plain; charset=utf-8"), ("Access-Control-Allow-Origin","*")]
                "OK"
  where 
    decode1 :: S8.ByteString -> Maybe UserInformation
    decode1 = decode


