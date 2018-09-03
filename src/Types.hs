-- This file include UserInformation and UserInformationList data type declaration, and their FromJSON/ToJSON instances.

-- Enable overloading od string literals using a type class, much like integer literals.
{-# LANGUAGE OverloadedStrings          #-}
-- This allows us to deriving Generic, and use auto-generated generic instances. 
{-# LANGUAGE DeriveGeneric              #-}
-- This allows us to write a data type like 'UserInformation{..}' when using it as the input of a function or instance, 
-- then we can directly use its functions (userName, userPhone, etc.) as data instead using 'userName (ui :: UserInformation)'. see #31
{-# LANGUAGE RecordWildCards            #-}

module Types
    ( UserInformation (..)
    , UserInformationList (..)
    ) where 

import Data.Aeson 
import Data.Text (Text)
import Data.Time.Clock 
import GHC.Generics (Generic)

data UserInformation = UserInformation
    { userName    :: !Text
    , userPhone   :: !(Maybe Text)
    , userEmail   :: !(Maybe Text)
    , userMessage :: !Text
    , timeStamp   :: !(Maybe UTCTime)
    } deriving (Show)

--instance ToJSON UserInformation
instance ToJSON UserInformation where
    toJSON UserInformation{..} = 
        object [ "name"    .= userName 
               , "phone"   .= userPhone 
               , "email"   .= userEmail 
               , "message" .= userMessage 
               , "time"    .= timeStamp
               ]

--instance FromJSON UserInformation
instance FromJSON UserInformation where 
    parseJSON = withObject "UserInformation" $ \o -> 
        UserInformation <$> o .:  "name"
                        <*> o .:? "phone"
                        <*> o .:? "email"
                        <*> o .:  "message" 
                        <*> o .:? "time"


newtype UserInformationList = UserInformationList
    { userInfoList :: [UserInformation]
    } deriving (Show, Generic)

instance FromJSON UserInformationList

instance ToJSON UserInformationList

