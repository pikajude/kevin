module Kevin.Protocol.IRC (
    cleanup,
    listen,
    errHandlers,
    getAuthInfo
) where

import qualified Data.ByteString.Char8 as B
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import Kevin.Packet.IRC
import Control.Monad.State

type KevinState = StateT Settings IO

cleanup :: KevinIO ()
cleanup = io $ klog Green "cleanup client"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- ask
    line <- io $ readClient k
    unless (B.null line) $ do
        io $ print line
        listen

errHandlers :: [Handler KevinIO ()]
errHandlers = [
    Handler (\(e :: KevinException) -> case e of
        LostServer -> io $ klogError "Lost server connection, DCing client"
        ParseFailure -> io $ klogError "Bad communication from client"
        _ -> io $ klogError "Got the wrong exception"),
        
    Handler (\(e :: IOException) -> do
        servId <- asks serverId
        io $ do
            klogError $ "client: " ++ show e
            tid <- takeMVar servId
            throwTo tid LostClient)
              ]

getAuthInfo :: Handle -> KevinState ()
getAuthInfo handle = do
    pkt <- io $ fmap parsePacket $ B.hGetLine handle
    io $ klog Blue $ "client <- " ++ B.unpack (asString pkt)
    case command pkt of
        "PASS" -> do
            modify (setAuthtoken $ head $ params pkt)
            getAuthInfo handle
        "NICK" -> do
            modify (setUsername $ head $ params pkt)
            getAuthInfo handle
        "USER" -> welcome handle
        _ -> io $ klogError $ "invalid packet: " ++ show pkt

welcome :: Handle -> KevinState ()
welcome handle = do
    nick <- gets username
    mapM_ (\x -> io $ (klog Blue $ "client -> " ++ B.unpack (asString x)) >> (B.hPut handle $ asString x)) [
        Packet { prefix = hostname
               , command = "001"
               , params = [nick, B.concat ["Welcome to dAmnServer ", nick, "!", nick, "@chat.deviantart.com"]]
               },
        Packet { prefix = hostname
               , command = "002"
               , params = [nick, "Your host is chat.deviantart.com, running dAmnServer 0.3"]
               },
        Packet { prefix = hostname
               , command = "003"
               , params = [nick, "This server was created Thu Apr 28 1994 at 05:30:00 EDT"]
               },
        Packet { prefix = hostname
               , command = "004"
               , params = [nick, "chat.deviantart.com", "dAmnServer0.3", "qaohv", "i"]
               },
        Packet { prefix = hostname
               , command = "375"
               , params = [nick, "- chat.deviantart.com Message of the day -"]
               },
        Packet { prefix = hostname
               , command = "372"
               , params = [nick, "- dAmn chat on IRC brought to you by kevin " `B.append` VERSION]
               },
        Packet { prefix = hostname
               , command = "376"
               , params = [nick, "End of MOTD command"]
               }
        ]
    where
        hostname = Just "chat.deviantart.com"