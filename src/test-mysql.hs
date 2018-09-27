{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Base
import Data.Word
import qualified System.IO.Streams as Streams


main :: IO ()
main = do 
    conn <- connect 
            ConnectInfo {ciHost = "db011e.feisu.space", ciPort = 3306, ciDatabase = "a0912112304", ciUser = "a0912112304", ciPassword = "f9739de6", ciCharset = 33}
    (defs, is) <- query_ conn "SELECT * FROM dede_addonarticle WHERE aid = 7"
    print  =<< Streams.toList is