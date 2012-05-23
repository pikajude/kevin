module Kevin.Settings (
    Settings(..),
    emptySettings,
    setUsername,
    setAuthtoken
) where

data Settings = Settings { username :: String
                         , authtoken :: String
                         }

emptySettings = Settings { username = ""
                         , authtoken = ""
                         }

setUsername  str (Settings _ auth) = Settings str auth
setAuthtoken str (Settings user _) = Settings user str