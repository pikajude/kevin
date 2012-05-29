module Kevin.Protocol (kevinServer) where

import Prelude hiding (putStrLn, catch)
import Kevin.Base
import Kevin.Util.Logger
import qualified Kevin.IRC.Protocol as C
import qualified Kevin.Damn.Protocol as S
import System.IO (hSetBuffering, BufferMode(..))
import Data.Monoid (mempty)

mkKevin :: Socket -> IO Kevin
mkKevin sock = withSocketsDo $ do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    klogNow Blue "received a client"
    set <- execStateT (C.getAuthInfo client False) emptySettings
    klogNow Blue $ "client info: " ++ show set
    damnSock <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damnSock NoBuffering
    logChan <- newChan
    return Kevin { damn = damnSock
                 , irc = client
                 , settings = set
                 , users = mempty
                 , privclasses = mempty
                 , titles = mempty
                 , toJoin = mempty
                 , loggedIn = False
                 , logger = logChan
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
    mvar <- newTVarIO kevin
    runLogger (logger kevin)
    forkIO $ evalStateT (bracket_ S.initialize (S.cleanup >> io (closeServer kevin) >> io (closeClient kevin)) S.listen) mvar
    forkIO $ evalStateT (bracket_ (return ()) (C.cleanup >> io (closeServer kevin) >> io (closeClient kevin)) C.listen) mvar
    return ()