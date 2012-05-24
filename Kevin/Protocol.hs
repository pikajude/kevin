module Kevin.Protocol (listen, mkKevin) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import qualified Kevin.Protocol.Client as C
import qualified Kevin.Protocol.Server as S
import qualified Data.ByteString as B
import Control.Monad (forever, unless)
import Control.Monad.State
import System.IO (hSetBuffering, BufferMode(..))

mkKevin :: Socket -> IO Kevin
mkKevin sock = withSocketsDo $ do
    (client, _, _) <- accept sock
    klog Blue "received a client"
    (authtoken, username) <- C.getAuthInfo client
    klog Blue $ "client info: " ++ username ++ ", " ++ authtoken
    damn <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damn NoBuffering
    hSetBuffering client NoBuffering
    clientId <- newEmptyMVar
    servId <- newEmptyMVar
    return Kevin { damn = damn
                 , irc = client
                 , serverId = servId
                 , clientId = clientId
                 , settings = Settings username authtoken
                 }

listen :: KevinIO ()
listen = do
    kevin <- ask
    liftIO $ do
        sid <- forkIO $ runReaderT (bracket_ S.initialize S.cleanup S.listen) kevin
        cid <- forkIO $ runReaderT (bracket_ (return ()) C.cleanup C.listen) kevin
        putMVar (serverId kevin) sid
        putMVar (clientId kevin) cid