module Kevin.Protocol (listen) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import qualified Kevin.Protocol.Client as C
import qualified Kevin.Protocol.Server as S
import System.IO (Handle(..), hClose, hIsClosed)
import Control.Exception.Base
import Control.Exception
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever, unless)

listen :: Kevin -> IO ()
listen kevin = do
    servId <- newEmptyMVar
    clientId <- newEmptyMVar
    sid <- forkIO $ bracket_ (S.initialize kevin)
                             (S.cleanup kevin)
                             (listenServer kevin clientId)
    cid <- forkIO $ bracket_ (C.initialize kevin)
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