module Kevin.Version (showVersion, version, versionStr) where

import Data.Text (Text, pack)
import Data.Version

version :: Version
version = Version [0,7,2] []

versionStr :: Text
versionStr = pack $ showVersion version
