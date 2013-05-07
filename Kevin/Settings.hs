module Kevin.Settings where

import Control.Lens
import Data.Default
import Data.Text

data Settings = Settings { _name      :: Text
                         , _password  :: Text
                         , _authtoken :: Text
                         , _passed    :: Bool
                         , _nicked    :: Bool
                         , _usered    :: Bool
                         } deriving (Show)

makeClassy ''Settings

instance Default Settings where
    def = Settings "" "" "" False False False
