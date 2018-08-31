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

-- Use functions (pack, unpack) and type (ByteString) of this package
import qualified Data.ByteString.Lazy.Char8 as S8

-- The main funtion
main :: IO ()
main = do
    -- Custom port number
    args <- getArgs
    let port = (read $ head args) :: Int 
    -- Create database tables if they don't exist. 
    databaseDir <- findFile ["../../../db"] "UserInfo.db"
    when (isNothing databaseDir) createTables
    -- Run the main app
    putStrLn "Server Started."
    run port app
    
-- The main app is used to handle pathInfo and method for requests. 
-- For path "get"  method "Get"  request, handleGetRequest  function will handle and process it.
-- For path "post" mehtod "Post" request, handlePostRequest function will handle and process it.

-- If the pathInfo is neither "get" nor "post", the app will return 404 "Not Found".
-- If the method is not "Get" for a "get"-path-request, or "Post" for a "post"-path-request, 
-- the app will return 405 "Method Not Allowed"

app :: Application   -- Application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req send = 
    case pathInfo req of 
        ["get"]  -> if requestMethod req == methodGet
                    then handleGetRequest req send
                    else send $ responseLBS   -- Use responseLBS builder to build the respose 
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

-- Handle "Get" request
handleGetRequest :: Application
handleGetRequest _ send = do   -- we don't need anything other thing in the request header or body
    -- Get the Users' Information (as :t UserInformationList) from database 
    usrInfoList <- getUserInfo  

    -- Encode them in standard json format.  UserInformationList -> Data.ByteString.Lazy.Char8.ByteString
    let responseByteString = encode usrInfoList :: S8.ByteString

    -- Show the json information in log
    putStrLn $ S8.unpack responseByteString

    -- Send the json as response, 200 OK
    send $ responseLBS 
            status200
            [("Content-Type", "application/json; charset=utf-8"),("Access-Control-Allow-Origin","*")]
            responseByteString




handlePostRequest :: Application 
handlePostRequest req send = do
    -- Get the body of the request and store it in ByteString
    reqBdy <- lazyRequestBody req 

    -- Show the  requst body in log
    putStrLn $ S8.unpack reqBdy

    -- For some unknown reasons(maybe something about charset encode?), we need to pack and
    -- unpack the ByteString, and then pass it to decode function to make it work normally
    let requestByteString = S8.pack $ S8.unpack reqBdy 

    -- Decode the ByteString into UserInformation
    -- Notice: 'decode' function may fail, that's why we need Maybe 
    let usrInfo = decode requestByteString :: Maybe UserInformation
    
    -- If decode success, then return 200 OK, else return 400 Bad Request
    case usrInfo of 
        Nothing -> do 
            putStrLn "usrInfo1 Nothing"
            send $ responseLBS
                status400
                [("Content-Type", "text/plain; charset=utf-8"), ("Access-Control-Allow-Origin","*")]
                "Bad request, please check your request body!"
        Just ui -> do
            -- Print the UserInformation for debugging
            print ui
            insertUserInfo ui
            send $ responseLBS
                status200
                [("Content-Type", "text/plain; charset=utf-8"), ("Access-Control-Allow-Origin","*")]
                "OK"



