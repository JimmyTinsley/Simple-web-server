{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Aeson
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Util
import Network.Wai.Handler.Warp (run)
import Types 

--import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.Char8 as S8

main :: IO ()
main = do
    -- port <- getArgs
    putStrLn "Server Started."
    run 8080 app
    

app :: Application
app req send = do 
    
    case pathInfo req of 
 --       ["get"]  -> send $ getForms 

        ["post"] -> saveResult req send 

        _        -> send $ responseLBS
            status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "Not found"


saveResult :: Application 
saveResult req send = do
    -- Get the body of the request and store it in ByteString
    reqBdy <- lazyRequestBody req 

    -- Show the  requst body in log
    putStrLn $ S8.unpack reqBdy

    -- For some unknown reasons(maybe something about charset encode?), we need to pack and
    -- unpack the ByteString, and then pass it to decode function to make it work normally
    let reqByteString = S8.pack $ S8.unpack reqBdy 

    -- The input and out types of decode function need to set, write it as decode1
    let usrInfo = (decode1 reqByteString) 
    
    -- If decode success, then return 200 OK, else return 400 Bad Request
    case usrInfo of 
        Nothing -> do 
            putStrLn "usrInfo1 Nothing"
            send $ responseLBS
                status400
                [("Content-Type", "text/plain; charset=utf-8")]
                "Bad Request"
        Just ui -> do
            -- Print the UserInformation for debugging
            print ui
            send $ responseLBS
                status200
                [("Content-Type", "text/plain; charset=utf-8")]
                "OK"
  where 
    decode1 :: S8.ByteString -> Maybe UserInformation
    decode1 = decode