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

setUsername  str set = set { username  = str }
setAuthtoken str set = set { authtoken = str }
setPassword  str set = set { password  = str }