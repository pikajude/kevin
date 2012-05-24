module Kevin.Protocol.Client (
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
    io $ B.hPut handle "Please enter your username: "
    pkt <- io $ fmap parsePacket $ B.hGetLine handle
    modify (setUsername $ head $ params pkt)