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

getAuthInfo :: Handle -> StateT Settings IO ()
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

welcome :: Handle -> StateT Settings IO ()
welcome handle = do
    nick <- gets username
    mapM_ (\x -> io $ (klog Blue $ "client -> " ++ B.unpack (asString x)) >> (B.hPut handle $ asString x)) [
        Packet { prefix = Just hostname
               , command = "001"
               , params = [nick, B.concat ["Welcome to dAmnServer ", nick, "!", nick, "@chat.deviantart.com"]] }
        ]
    where
        hostname = "chat.deviantart.com"