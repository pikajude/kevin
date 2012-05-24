module Kevin.Protocol (kevinServer) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import qualified Kevin.Protocol.Client as C
import qualified Kevin.Protocol.Server as S
import System.IO (hSetBuffering, BufferMode(..))

mkKevin :: Socket -> IO Kevin
mkKevin sock = withSocketsDo $ do
    (client, _, _) <- accept sock
    klog Blue "received a client"
    (user, auth) <- C.getAuthInfo client
    klog Blue $ "client info: " ++ user ++ ", " ++ auth
    damnSock <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damnSock NoBuffering
    hSetBuffering client NoBuffering
    cid <- newEmptyMVar
    sid <- newEmptyMVar
    return Kevin { damn = damnSock
                 , irc = client
                 , serverId = sid
                 , clientId = cid
                 , settings = Settings user auth
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