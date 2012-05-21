module Kevin.Base (
    Kevin(..),
    mkKevin,
    mkListener
) where

import Kevin.Util.Logger
import Data.ByteString.Char8 hiding (putStrLn)
import System.IO
import Network

data Kevin = Kevin { damn    :: Handle
                   , irc     :: Handle
                   }

mkListener :: IO Socket
mkListener = listenOn $ PortNumber 6669

mkKevin :: Socket -> IO Kevin
mkKevin sock = withSocketsDo $ do
    (client, _, _) <- accept sock
    klog Blue "received a client"
    damn <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damn NoBuffering
    hSetBuffering client NoBuffering
    return Kevin { damn = damn
                 , irc = client
                 }