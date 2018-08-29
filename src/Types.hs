{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}

module Types
    ( UserInformation (..)
    , UserInformationList (..)
    )where 

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
    } deriving (Show, Generic)

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

