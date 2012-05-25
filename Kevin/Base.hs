module Kevin.Base (
    Kevin(..),
    KevinIO,
    KevinException(..),
    readClient,
    readServer,
    writeClient,
    writeServer,
    module K,
    io
) where

import Kevin.Util.Logger
import Kevin.Settings
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import Data.List (intercalate)
import System.IO as K (Handle)
import Control.Exception as K (IOException)
import Network as K
import Control.Monad.Reader as K
import Control.Concurrent as K
import Control.Monad.CatchIO as K

class KevinServer a where
    readClient, readServer :: a -> IO B.ByteString
    writeClient, writeServer :: a -> B.ByteString -> IO ()

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   , serverId :: MVar ThreadId
                   , clientId :: MVar ThreadId
                   , settings :: Settings
                   }

type KevinIO = ReaderT Kevin IO

io :: MonadIO m => IO a -> m a
io = liftIO
                   
padLines :: Int -> B.ByteString -> String
padLines len b = let (first:rest) = Prelude.lines $ B.unpack b in (++) (first ++ "\n") $ Data.List.intercalate "\n" $ Prelude.map (Prelude.replicate len ' ' ++) rest

hGetSep :: Char -> Handle -> IO B.ByteString
hGetSep sep h = do
    ch <- fmap B.head $ B.hGet h 1
    if ch == sep
        then return $ B.singleton sep
        else do
            nextch <- hGetSep sep h
            return $ B.cons ch nextch

instance KevinServer Kevin where
    readClient k = do
        line <- B.hGetLine $ irc k
        klog Blue $ "client <- " ++ padLines 10 line
        return $ B.init line
    readServer k = do
        line <- hGetSep '\x0' $ damn k
        klog Magenta $ "server <- " ++ padLines 10 line
        return line
    
    writeClient k str = do
        klog Blue $ "client -> " ++ padLines 10 str
        B.hPut (irc k) str
    writeServer k str = do
        klog Magenta $ "server -> " ++ padLines 10 str
        B.hPut (damn k) str

data KevinException = LostClient | LostServer | ParseFailure
    deriving (Show, Typeable)

instance Exception KevinException