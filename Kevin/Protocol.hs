module Kevin.Protocol (kevinServer) where

import Prelude hiding (catch)
import Kevin.Base
import Kevin.Util.Logger
import qualified Control.Exception as E
import qualified Kevin.IRC.Protocol as C
import qualified Kevin.Damn.Protocol as S
import System.IO (hSetBuffering, BufferMode(..))
import Control.Monad.State
import Data.Monoid (mempty)

watchInterrupt :: [E.Handler (Maybe Kevin)]
watchInterrupt = [E.Handler (\(e :: E.AsyncException) -> throw e),
                  E.Handler (\(_ :: E.SomeException) -> return Nothing)]

mkKevin :: Socket -> IO (Maybe Kevin)
mkKevin sock = flip E.catches watchInterrupt . withSocketsDo $ do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    klogNow Blue "received a client"
    set <- execStateT (C.getAuthInfo client False) emptySettings
    damnSock <- connectTo "chat.deviantart.com" $ PortNumber 3900
    hSetBuffering damnSock NoBuffering
    logChan <- newChan
    damnChan <- newChan
    ircChan <- newChan
    return $ Just Kevin { damn = damnSock
                        , irc = client
                        , dChan = damnChan
                        , iChan = ircChan
                        , settings = set
                        , users = mempty
                        , privclasses = mempty
                        , titles = mempty
                        , toJoin = mempty
                        , joining = mempty
                        , loggedIn = False
                        , logger = logChan
                        }

mkListener :: Int -> IO Socket
mkListener = listenOn . PortNumber . fromIntegral

kevinServer :: Int -> IO ()
kevinServer n = do
    sock <- mkListener n
    putStrLn $ "Listening on port " ++ show n
    forever $ do
        kev <- mkKevin sock
        case kev of
            Just k -> listen k
            Nothing -> return ()

listen :: Kevin -> IO ()
listen kevin = do
    mvar <- newTVarIO kevin
    runLogger (logger kevin)
    runPrinter (dChan kevin) (damn kevin)
    runPrinter (iChan kevin) (irc kevin)
    forkIO . void $ runReaderT (bracket_ S.initialize (S.cleanup >> io (closeClient kevin)) S.listen) mvar
    forkIO . void $ runReaderT (bracket_ (return ()) (C.cleanup >> io (closeServer kevin)) C.listen) mvar
    return ()