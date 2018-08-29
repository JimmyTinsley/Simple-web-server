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
    , userEmail   :: !(Maybe Text)
    , userMessage :: !Text
    } deriving (Show, Generic)

--instance ToJSON UserInformation
instance ToJSON UserInformation where
    toJSON (UserInformation userName' userPhone' userEmail' userMessage') = 
        object [ "name"    .= userName' 
               , "phone"   .= userPhone' 
               , "email"   .= userEmail' 
               , "message" .= userMessage' 
               ]

--instance FromJSON UserInformation
instance FromJSON UserInformation where 
    parseJSON = withObject "UserInformation" $ \o -> 
        UserInformation <$> o .:  "name"
                        <*> o .:? "phone"
                        <*> o .:?　"email"
                        <*> o .: "message" 


newtype UserInformationList = UserInformationList
    { userInfoList :: [UserInformation]
    } deriving (Show, Generic)

instance FromJSON UserInformationList

instance ToJSON UserInformationList
