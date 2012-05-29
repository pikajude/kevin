module Kevin.Base (
    Kevin(..),
    KevinIO,
    KevinException(..),
    KServerPacket(..),
    KClientPacket(..),
    
    -- * Kevin IO actions
    readClient,
    readServer,
    writeClient,
    writeServer,
    closeClient,
    closeServer,
    
    -- * Privclasses
    setPrivclass,
    onPrivclasses,
    Privclass,
    -- * Other
    logIn,
    addToJoin,
    
    -- * Exports
    module K,
    
    -- * Working with KevinState
    io,
    getK,
    putK,
    getsK,
    modifyK
) where

import Kevin.Util.Logger
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import Data.List (intercalate, nub)
import System.IO as K (Handle, hClose)
import Control.Exception as K (IOException)
import Network as K
import Control.Monad.Reader
import Control.Monad.State as K
import Control.Monad.STM (atomically)
import Control.Concurrent as K (forkIO)
import Control.Concurrent.STM.TVar as K
import Control.Monad.CatchIO as K
import Kevin.Settings as K
import qualified Data.Map as M

class KServerPacket a where
    asStringS :: a -> B.ByteString

class KClientPacket a where
    asStringC :: a -> B.ByteString

class KevinServer a where
    readClient, readServer :: a -> IO B.ByteString
    writeServer :: (KServerPacket p) => a -> p -> IO ()
    writeClient :: (KClientPacket p) => a -> p -> IO ()
    closeClient, closeServer :: a -> IO ()

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   , settings :: Settings
                   , privclasses :: PrivclassStore
                   , toJoin :: [B.ByteString]
                   , loggedIn :: Bool
                   }

type Privclass = (B.ByteString, Int)
type KevinIO = StateT (TVar Kevin) IO

getK :: KevinIO Kevin
getK = get >>= io . readTVarIO

putK :: Kevin -> KevinIO ()
putK k = get >>= io . atomically . flip writeTVar k

getsK :: (Kevin -> a) -> KevinIO a
getsK = flip liftM getK

modifyK :: (Kevin -> Kevin) -> KevinIO ()
modifyK f = do
    var <- get
    (io . atomically) $ modifyTVar var f

io :: MonadIO m => IO a -> m a
io = liftIO
                   
padLines :: Int -> B.ByteString -> String
padLines len b = let (first:rest) = lines $ B.unpack b in (++) (first ++ "\n") $ intercalate "\n" $ map (replicate len ' ' ++) rest

hGetSep :: Char -> Handle -> IO B.ByteString
hGetSep sep h = do
    ch <- fmap B.head $ B.hGet h 1
    if ch == sep
        then return ""
        else do
            nextch <- hGetSep sep h
            return $ B.cons ch nextch

instance KevinServer Kevin where
    readClient k = do
        line <- B.hGetLine $ irc k
        klog Yellow $ "client <- " ++ padLines 10 line
        return $ B.init line
    readServer k = do
        line <- hGetSep '\x0' $ damn k
        klog Cyan $ "server <- " ++ padLines 10 line
        return line
    
    writeClient k str = do
        let pkt = asStringC str
        klog Blue $ "client -> " ++ padLines 10 pkt
        B.hPut (irc k) pkt
    writeServer k str = do
        let pkt = asStringS str
        klog Magenta $ "server -> " ++ padLines 10 pkt
        B.hPut (damn k) pkt
    
    closeClient = hClose . irc
    closeServer = hClose . damn

instance KServerPacket B.ByteString where
    asStringS = id

instance KClientPacket B.ByteString where
    asStringC = id

data KevinException = LostClient | LostServer | ParseFailure
    deriving (Show, Typeable)

instance Exception KevinException

-- Kevin modifiers
logIn :: Kevin -> Kevin
logIn k = k { loggedIn = True }

addToJoin :: [B.ByteString] -> Kevin -> Kevin
addToJoin rooms k = k { toJoin = nub $ rooms ++ toJoin k }

type Chatroom = B.ByteString
type Privclasses = M.Map B.ByteString Int
type PrivclassStore = M.Map Chatroom Privclasses

setPrivclass :: Chatroom -> Privclass -> PrivclassStore -> PrivclassStore
setPrivclass room (p,i) = M.insertWith M.union room (M.singleton p i)

onPrivclasses :: (PrivclassStore -> PrivclassStore) -> Kevin -> Kevin
onPrivclasses f k = k { privclasses = f (privclasses k) }