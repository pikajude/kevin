module Kevin.Base (
    Kevin(..),
    KevinIO(..),
    KevinException(..),
    mkListener,
    readClient,
    readServer,
    writeClient,
    writeServer,
    module B,
    module K,
    io
) where

import Kevin.Util.Logger
import Kevin.Settings
import Data.ByteString.Char8 as B
import Data.Typeable
import Data.List (intercalate)
import System.IO as K (Handle(..))
import Control.Exception as K (Exception(..), IOException(..))
import Network as K
import Control.Monad.Reader as K
import Control.Concurrent as K
import Control.Concurrent.MVar as K
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

io :: IO a -> KevinIO a
io = liftIO
                   
padLines :: Int -> B.ByteString -> String
padLines len b = let (first:rest) = Prelude.lines $ B.unpack b in (++) (first ++ "\n") $ Data.List.intercalate "\n" $ Prelude.map (Prelude.replicate len ' ' ++) rest

instance KevinServer Kevin where
    readClient k = do
        line <- B.hGetLine $ irc k
        klog Blue $ "client <- " ++ padLines 10 line
        return line
    readServer k = do
        line <- B.hGetLine $ damn k
        klog Magenta $ "server <- " ++ padLines 10 line
        return line
    
    writeClient k str = do
        klog Blue $ "client -> " ++ padLines 10 str
        B.hPut (irc k) str
    writeServer k str = do
        klog Magenta $ "server -> " ++ padLines 10 str
        B.hPut (damn k) str

data KevinException = LostClient | LostServer
    deriving (Show, Typeable)

instance Exception KevinException

mkListener :: IO Socket
mkListener = listenOn $ PortNumber 6669