{-# LANGUAGE ScopedTypeVariables #-}

module Kevin.Base (
    Kevin(..),
    mkKevin,
    mkListener,
    readClient,
    readServer,
    writeClient,
    writeServer
) where

import Prelude hiding (catch)
import Kevin.Util.Logger
import qualified Data.ByteString.Char8 as B
import System.IO
import Control.Exception
import Network

class KevinServer a where
    readClient, readServer :: a -> IO B.ByteString
    writeClient, writeServer :: a -> B.ByteString -> IO ()

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   }

instance KevinServer Kevin where
    readClient (Kevin damn irc) = B.hGetLine irc
    readServer (Kevin damn irc) = B.hGetLine damn
    
    writeClient (Kevin damn irc) = B.hPut irc
    writeServer (Kevin damn irc) = B.hPut damn

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