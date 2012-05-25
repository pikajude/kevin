module Kevin.Protocol (kevinServer) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import qualified Kevin.Protocol.IRC as C
import qualified Kevin.Protocol.Damn as S
import System.IO (hSetBuffering, BufferMode(..))
import Control.Monad.State

mkKevin :: Socket -> IO Kevin
mkKevin sock = withSocketsDo $ do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    klog Blue "received a client"
    set <- execStateT (C.getAuthInfo client) emptySettings
    klog Blue $ "client info: " ++ show set
    damnSock <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damnSock NoBuffering
    cid <- newEmptyMVar
    sid <- newEmptyMVar
    return Kevin { damn = damnSock
                 , irc = client
                 , serverId = sid
                 , clientId = cid
                 , settings = set
                 }

mkListener :: IO Socket
mkListener = listenOn $ PortNumber 6669

kevinServer :: IO ()
kevinServer = do
    sock <- mkListener
    forever $ do
        kev <- mkKevin sock
        listen kev

listen :: Kevin -> IO ()
listen kevin = do
        sid <- forkIO $ runReaderT (bracket_ S.initialize S.cleanup S.listen) kevin
        cid <- forkIO $ runReaderT (bracket_ (return ()) C.cleanup C.listen) kevin
        putMVar (serverId kevin) sid
        putMVar (clientId kevin) cid