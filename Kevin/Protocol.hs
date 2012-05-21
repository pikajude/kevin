{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Kevin.Protocol (listen) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import System.IO (Handle(..))
import Control.Exception.Base (IOException)
import Control.Exception (catch)
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)

listen :: Kevin -> IO ()
listen kevin = do
    servId <- newEmptyMVar
    clientId <- newEmptyMVar
    sid <- forkIO $ listenServer kevin clientId
    cid <- forkIO $ listenClient kevin servId
    putMVar servId sid
    putMVar clientId cid

swallow :: IO () -> IO ()
swallow act = catch act (const . return ())

cleanupClient :: Kevin -> IO ()
cleanupClient (Kevin damn irc) = swallow (C.writeFinishers irc) >> hClose irc

cleanupServer :: Kevin -> IO ()
cleanupServer (Kevin damn irc) = swallow (S.writeFinishers damn) >> hClose damn

getLineOrKill :: Handle -> MVar ThreadId -> IO B.ByteString
getLineOrKill h t = do
    line <- B.hGetLine h `catch` (\(e :: IOException) -> do
        klog Red "Lost connection to client"
        tid <- takeMVar t
        killThread tid
        return "")
    return line

listenClient :: Kevin -> MVar ThreadId -> IO ()
listenClient k@(Kevin damn irc) servId = do
    line <- getLineOrKill irc servId
    if not $ B.null line then do
        print line
        listenClient k servId
    else
        cleanupClient k

listenServer :: Kevin -> MVar ThreadId -> IO ()
listenServer k@(Kevin damn irc) clId = do
    line <- B.hGetLine damn
    print line
    listenServer k clId