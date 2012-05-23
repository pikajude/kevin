module Kevin.Protocol (listen, mkKevin) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import qualified Kevin.Protocol.Client as C
import qualified Kevin.Protocol.Server as S
import Control.Exception.Base
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.MVar
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
    return Kevin { damn = damn
                 , irc = client
                 , settings = Settings username authtoken
                 }

listen :: ReaderT Kevin IO ()
listen = do
    kevin <- ask
    liftIO $ do
        servId <- newEmptyMVar
        clientId <- newEmptyMVar
        sid <- forkIO $ bracket_ (S.initialize kevin)
                                 (S.cleanup kevin)
                                 (listenServer kevin clientId)
        cid <- forkIO $ bracket_ (return ())
                                 (C.cleanup kevin)
                                 (listenClient kevin servId)
        putMVar servId sid
        putMVar clientId cid

listenClient :: Kevin -> MVar ThreadId -> IO ()
listenClient k servId = flip catches C.errHandlers $ do
    line <- readClient k `catch` (\(e :: IOException) -> do
        klogError $ "client: " ++ show e
        tid <- takeMVar servId
        throwTo tid LostClient
        return "")
    unless (B.null line) $ do
        print line
        listenClient k servId

listenServer :: Kevin -> MVar ThreadId -> IO ()
listenServer k clId = flip catches S.errHandlers $ do
    line <- readServer k `catch` (\(e :: IOException) -> do
        klogError $ "server: " ++ show e
        tid <- takeMVar clId
        throwTo tid LostServer
        return "")
    unless (B.null line) $ do
        print line
        listenServer k clId