module Kevin.Base (
    Kevin(..),
    KevinIO,
    KevinException(..),
    KServerPacket(..),
    KClientPacket(..),
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

class KServerPacket a where
    asStringS :: a -> B.ByteString

class KClientPacket a where
    asStringC :: a -> B.ByteString

class KevinServer a where
    readClient, readServer :: a -> IO B.ByteString
    writeServer :: (KServerPacket p) => a -> p -> IO ()
    writeClient :: (KClientPacket p) => a -> p -> IO ()

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
        let pkt = asStringC str
        klog Blue $ "client -> " ++ padLines 10 pkt
        B.hPut (irc k) pkt
    writeServer k str = do
        let pkt = asStringS str
        klog Magenta $ "server -> " ++ padLines 10 pkt
        B.hPut (damn k) pkt

instance KServerPacket B.ByteString where
    asStringS = id

instance KClientPacket B.ByteString where
    asStringC = id

data KevinException = LostClient | LostServer | ParseFailure
    deriving (Show, Typeable)

instance Exception KevinException