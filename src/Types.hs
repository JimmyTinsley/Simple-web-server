{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Types
    ( UserInformation (..)
    , UserInformationList (..)
    )where 

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data UserInformation = UserInformation
    { userName    :: !Text
    , userPhone   :: !(Maybe Text)
    , userEmail   :: !Text
    , userMessage :: !Text
    } deriving (Show, Generic)

--instance ToJSON UserInformation
instance ToJSON UserInformation where
    toJSON (UserInformation userName1 userPhone1 userEmail1 userMessage1) = 
        object [ "name"    .= userName1 
               , "phone"   .= userPhone1 
               , "email"   .= userEmail1 
               , "message" .= userMessage1 
               ]

--instance FromJSON UserInformation
instance FromJSON UserInformation where 
    parseJSON = withObject "UserInformation" $ \o -> 
        UserInformation <$> o .:  "name"
                        <*> o .:? "phone"
                        <*> o .:　"email"
                        <*> o .: "message" 


newtype UserInformationList = UserInformationList
    { userInfoList :: [UserInformation]
    } deriving (Show, Generic)

instance FromJSON UserInformationList

instance ToJSON UserInformationList
