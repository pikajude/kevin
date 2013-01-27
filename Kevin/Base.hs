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

import Control.Applicative ((<$>))
import Control.Concurrent as K (forkIO)
import Control.Concurrent.Chan as K
import Control.Concurrent.STM.TVar as K
import Control.Exception
import Control.Exception as K (IOException)
import Control.Exception.Lens
import Control.Lens as K
import Control.Monad.CatchIO as K
import Control.Monad.Reader as K
import qualified Data.ByteString.Char8 as T (hGetLine, hPutStr)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Kevin.Chatrooms as K
import Kevin.Settings as K
import Kevin.Types
import Kevin.Util.Logger
import Network as K
import System.IO as K
import System.IO.Error

runPrinter :: Chan T.Text -> Handle -> IO ()
runPrinter ch h = void . forkIO . forever $ readChan ch >>= T.hPutStr h . T.encodeUtf8

io :: MonadIO m => IO a -> m a
io = liftIO

class KevinServer a where
    readClient, readServer :: a -> IO T.Text
    writeServer :: a -> T.Text -> IO ()
    writeClient :: a -> T.Text -> IO ()
    closeClient, closeServer :: a -> IO ()

data KevinException = ParseFailure
    deriving (Show, Typeable)

instance Exception KevinException

_KevinException :: Prism' SomeException KevinException
_KevinException = exception

-- actions

padLines :: Int -> T.Text -> String
padLines len b = let (first:rest) = lines $ T.unpack b
                  in (++) (first ++ "\n") . intercalate "\n"
                          . map (replicate len ' ' ++)
                        $ rest

hGetCharTimeout :: Handle -> Int -> IO Char
hGetCharTimeout h t = do
    hSetBuffering h NoBuffering
    ready <- hWaitForInput h t
    if ready
        then do
            c <- hGetChar h
            return c
        else throwIO $ mkIOError eofErrorType "read timeout" (Just h) Nothing

hGetSep :: Char -> Handle -> IO String
hGetSep sep h = fix (\f -> do
    ch <- hGetCharTimeout h 180000
    if ch == sep
        then return ""
        else (ch:) <$> f)

instance KevinServer Kevin where
    readClient k = do
        line <- T.decodeUtf8 <$> T.hGetLine (irc k)
        klog_ (logger k) Yellow $ "client <- " ++ padLines 10 line
        return $ T.init line
    readServer k = do
        line <- T.pack <$> hGetSep '\NUL' (damn k)
        klog_ (logger k) Cyan $ "server <- " ++ padLines 10 line
        return line

    writeClient k pkt = do
        klog_ (logger k) Blue $ "client -> " ++ padLines 10 pkt
        writeChan (iChan k) pkt
    writeServer k pkt = do
        klog_ (logger k) Magenta $ "server -> " ++ padLines 10 pkt
        writeChan (dChan k) pkt

    closeClient = hClose . irc
    closeServer = hClose . damn
