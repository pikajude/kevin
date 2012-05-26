module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken,
    setPassword
) where
    
import Data.ByteString.Char8

data Settings = Settings { username :: ByteString
                         , password :: ByteString
                         , authtoken :: ByteString
                         } deriving (Show)

emptySettings :: Settings
emptySettings = Settings { username = ""
                         , password = ""
                         , authtoken = ""
                         }

setUsername, setAuthtoken, setPassword :: ByteString -> Settings -> Settings

setUsername  str (Settings _ pass auth) = Settings str pass auth
setAuthtoken str (Settings user pass _) = Settings user pass str
setPassword  str (Settings user _ auth) = Settings user str auth