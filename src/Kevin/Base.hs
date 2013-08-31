{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Kevin.Base (
    module Kevin.Types,
    KevinException(..),
    _KevinException,
    KevinServer(..),
    User(..),

    module K,

    io,
    runPrinter,

    printf
) where

import           Control.Applicative              ((<$>))
import           Control.Concurrent as K          (forkIO)
import           Control.Concurrent.Chan as K
import           Control.Concurrent.STM.TVar as K
import           Control.Exception
import           Control.Exception as K           (IOException)
import           Control.Exception.Lens
import           Control.Lens as K
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Control.Monad.Catch as K
#else
import           Control.Monad.CatchIO as K
#endif
import           Control.Monad.Reader as K
import qualified Data.ByteString.Char8 as T       (hGetLine, hPutStr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Kevin.Chatrooms as K
import           Kevin.Settings as K
import           Kevin.Types
import           Kevin.Util.Logger
import           Network as K
import           System.IO as K
import           System.IO.Error
import qualified Text.Printf as P

runPrinter :: Chan T.Text -> Handle -> IO ()
runPrinter ch h = void . forkIO . forever $ readChan ch >>= T.hPutStr h . T.encodeUtf8

io :: MonadIO m => IO a -> m a
io = liftIO

class KevinServer a where
    readClient, readServer   :: a -> IO T.Text
    writeClient, writeServer :: a -> T.Text -> IO ()
    closeClient, closeServer :: a -> IO ()

data KevinException = ServerParseFailure String T.Text
                    | ClientParseFailure String T.Text
                    | ServerClosed
                    | ClientClosed
                    deriving Typeable

instance Show KevinException where
    show (ServerParseFailure reason input) =
        P.printf "Parse failure (%s) on packet from server:\n%s" reason (show input)
    show (ClientParseFailure reason input) =
        P.printf "Parse failure (%s) on packet from client:\n%s" reason (show input)
    show ServerClosed = "Server closed connection"
    show ClientClosed = "Client closed connection"

instance Exception KevinException

_KevinException :: Prism' SomeException KevinException
_KevinException = exception

-- actions

hGetCharTimeout :: Handle -> Int -> IO Char
hGetCharTimeout h t = do
    hSetBuffering h NoBuffering
    ready <- hWaitForInput h t
    if ready
        then hGetChar h
        else throwIO $ mkIOError eofErrorType "read timeout" (Just h) Nothing

hGetSep :: Char -> Handle -> IO String
hGetSep sep h = fix $ \f -> do
                    ch <- hGetCharTimeout h 180000
                    if ch == sep
                        then return ""
                        else (ch:) <$> f

instance KevinServer Kevin where
    readClient k = do
        line <- T.decodeUtf8 <$> T.hGetLine (irc k)
        return $ T.init line
    readServer k = T.pack <$> hGetSep '\NUL' (damn k)

    writeClient k = writeChan (iChan k)
    writeServer k = writeChan (dChan k)

    closeClient = hClose . irc
    closeServer = hClose . damn
