module Kevin.Base (
    Kevin(..),
    KevinException(..),
    mkListener,
    readClient,
    readServer,
    writeClient,
    writeServer,
    module System.IO,
    module Control.Exception,
    module Network,
    module B,
    module Control.Monad.Reader
) where

import Prelude hiding (catch)
import Kevin.Util.Logger
import Kevin.Settings
import qualified Data.ByteString.Char8 as B
import System.IO (Handle(..))
import Control.Exception
import Network
import Data.Typeable
import Data.List (intercalate)
import Control.Monad.Reader

class KevinServer a where
    readClient, readServer :: a -> IO B.ByteString
    writeClient, writeServer :: a -> B.ByteString -> IO ()

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   , settings :: Settings
                   }
                   
padLines :: Int -> B.ByteString -> String
padLines len b = let (first:rest) = lines $ B.unpack b in (++) (first ++ "\n") $ intercalate "\n" $ map (replicate len ' ' ++) rest

instance KevinServer Kevin where
    readClient (Kevin damn irc _) = do
        line <- B.hGetLine irc
        klog Blue $ "client <- " ++ padLines 10 line
        return line
    readServer (Kevin damn irc _) = do
        line <- B.hGetLine damn
        klog Magenta $ "server <- " ++ padLines 10 line
        return line
    
    writeClient (Kevin damn irc _) str = do
        klog Blue $ "client -> " ++ padLines 10 str
        B.hPut irc str
    writeServer (Kevin damn irc _) str = do
        klog Magenta $ "server -> " ++ padLines 10 str
        B.hPut damn str

data KevinException = LostClient | LostServer
    deriving (Show, Typeable)

instance Exception KevinException

mkListener :: IO Socket
mkListener = listenOn $ PortNumber 6669