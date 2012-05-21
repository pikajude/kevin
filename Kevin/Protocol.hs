{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

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
import Data.Typeable

data KevinException = ClientInterrupt
    deriving (Show, Typeable)

instance Exception KevinException

listen :: Kevin -> IO ()
listen kevin@(Kevin damn irc) = do
    servId <- newEmptyMVar
    clientId <- newEmptyMVar
    sid <- forkIO $ bracket_ (S.initialize damn)
                             (S.cleanup damn)
                             (listenServer kevin clientId)
    cid <- forkIO $ bracket_ (C.initialize irc)
                             (C.cleanup irc)
                             (listenClient kevin servId)
    putMVar servId sid
    putMVar clientId cid

listenClient :: Kevin -> MVar ThreadId -> IO ()
listenClient k servId = do
    line <- readClient k `catch` (\(e :: IOException) -> do
        klog Red $ "Client error: " ++ show e
        tid <- takeMVar servId
        throwTo tid ClientInterrupt
        return "")
    unless (B.null line) $ do
        print line
        listenClient k servId

listenServer :: Kevin -> MVar ThreadId -> IO ()
listenServer k clId = do
    line <- readServer k `catch` (\(e :: IOException) -> do
        klog Red $ "Server error: " ++ show e
        return "")
    unless (B.null line) $ do
        print line
        listenServer k clId