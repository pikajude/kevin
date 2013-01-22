{-# LANGUAGE TemplateHaskell #-}
module Kevin.Settings where

import Control.Lens
import Data.Text

data Settings = Settings { _name      :: Text
                         , _password  :: Text
                         , _authtoken :: Text
                         , _passed :: Bool
                         , _nicked :: Bool
                         , _usered :: Bool
                         } deriving (Show)

makeClassy ''Settings

emptySettings :: Settings
emptySettings = Settings "" "" "" False False False
