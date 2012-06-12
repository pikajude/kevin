module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken,
    setPassword,
    setHasPassed,
    setHasNicked,
    setHasUsered
) where
    
import Data.Text

data Settings = Settings { getUsername :: Text
                         , getPassword :: Text
                         , getAuthtoken :: Text
                         , hasPassed :: Bool
                         , hasNicked :: Bool
                         , hasUsered :: Bool
                         } deriving (Show)

emptySettings :: Settings
emptySettings = Settings { getUsername = ""
                         , getPassword = ""
                         , getAuthtoken = ""
                         , hasPassed = False
                         , hasNicked = False
                         , hasUsered = False
                         }

setUsername, setAuthtoken, setPassword :: Text -> Settings -> Settings
setHasPassed, setHasNicked, setHasUsered :: Settings -> Settings

setUsername  str set = set { getUsername  = str }
setAuthtoken str set = set { getAuthtoken = str }
setPassword  str set = set { getPassword  = str }
setHasPassed set = set { hasPassed = True }
setHasNicked set = set { hasNicked = True }
setHasUsered set = set { hasUsered = True }