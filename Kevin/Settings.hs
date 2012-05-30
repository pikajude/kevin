module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken,
    setPassword
) where
    
import Data.ByteString.Char8

data Settings = Settings { getUsername :: ByteString
                         , getPassword :: ByteString
                         , getAuthtoken :: ByteString
                         } deriving (Show)

emptySettings :: Settings
emptySettings = Settings { getUsername = ""
                         , getPassword = ""
                         , getAuthtoken = ""
                         }

setUsername, setAuthtoken, setPassword :: ByteString -> Settings -> Settings

setUsername  str set = set { getUsername  = str }
setAuthtoken str set = set { getAuthtoken = str }
setPassword  str set = set { getPassword  = str }