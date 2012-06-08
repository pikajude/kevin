module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken,
    setPassword
) where
    
import Data.Text

data Settings = Settings { getUsername ∷ Text
                         , getPassword ∷ Text
                         , getAuthtoken ∷ Text
                         } deriving (Show)

emptySettings ∷ Settings
emptySettings = Settings { getUsername = ""
                         , getPassword = ""
                         , getAuthtoken = ""
                         }

setUsername, setAuthtoken, setPassword ∷ Text → Settings → Settings

setUsername  str set = set { getUsername  = str }
setAuthtoken str set = set { getAuthtoken = str }
setPassword  str set = set { getPassword  = str }
