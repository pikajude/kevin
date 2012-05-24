module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken
) where
    
import Data.ByteString.Char8

data Settings = Settings { username :: ByteString
                         , authtoken :: ByteString
                         } deriving (Show)

emptySettings :: Settings
emptySettings = Settings { username = ""
                         , authtoken = ""
                         }

setUsername, setAuthtoken :: ByteString -> Settings -> Settings

setUsername  str (Settings _ auth) = Settings str auth
setAuthtoken str (Settings user _) = Settings user str