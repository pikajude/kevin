{-# LANGUAGE FlexibleContexts #-}

module Kevin.Protocol (kevinServer) where

import           Control.Concurrent
import           Control.Exception        (throwIO)
import           Control.Exception.Lens
import           Control.Monad.State
import           Data.Default
import           Data.Monoid              (mempty)
import           Kevin.Base
import qualified Kevin.Damn.Protocol as S
import qualified Kevin.IRC.Protocol as C
import           Kevin.Util.Logger
import           Prelude

watchInterrupt :: [Handler IO (Maybe Kevin)]
watchInterrupt = [ handler _AsyncException throwIO
                 , handler_ id (return Nothing) ]

mkKevin :: Socket -> IO (Maybe Kevin)
mkKevin sock = flip catches watchInterrupt . withSocketsDo
    $ do (client, _, _) <- accept sock
         hSetBuffering client NoBuffering
         klogNow Blue "received a client"
         s <- execStateT (C.getAuthInfo client False) def
         damnSock <- connectTo "chat.deviantart.com" $ PortNumber 3900
         hSetBuffering damnSock NoBuffering
         logChan <- newChan
         damnChan <- newChan
         ircChan <- newChan
         smv <- newEmptyMVar
         cmv <- newEmptyMVar
         return . Just $ Kevin damnSock
                               client
                               damnChan
                               ircChan
                               smv
                               cmv
                               s
                               mempty
                               mempty
                               mempty
                               mempty
                               False
                               logChan

mkListener :: Int -> IO Socket
mkListener = listenOn . PortNumber . fromIntegral

kevinServer :: Int -> IO ()
kevinServer n = do
    sock <- mkListener n
    putStrLn $ "Listening on port " ++ show n
    forever $ do
        kev <- mkKevin sock
        case kev of Just k -> listen k
                    Nothing -> return ()

listen :: Kevin -> IO ()
listen k = do
    mvar <- newTVarIO k
    runLogger (logger k)
    runPrinter (dChan k) (damn k)
    runPrinter (iChan k) (irc k)
    sId <- forkIO . void $ runReaderT (bracket_ S.initialize (S.cleanup >> io (closeServer k)) S.listen) mvar
    cId <- forkIO . void $ runReaderT (bracket_ (return ()) (C.cleanup >> io (closeClient k)) C.listen) mvar
    putMVar (serverMv k) sId
    putMVar (clientMv k) cId
    return ()
