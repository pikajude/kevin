module Kevin.Protocol.Damn (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Packet.Damn
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Kevin.Protocol.Damn.Send

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = io $ klog Blue "cleanup server"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- getK
    pkt <- io $ fmap parsePacket $ readServer k
    respond pkt (command pkt)
    listen

respond :: Packet -> B.ByteString -> KevinIO ()
respond _ "dAmnServer" = do
    set <- getsK settings
    let uname = username set
        token = authtoken set
    sendLogin uname token

respond pkt "login" = do
    uname <- getsK (username . settings)
    if okay pkt
        then do
            io $ klog Green $ "Logged in as " ++ B.unpack uname
            modifyK logIn
            getsK toJoin >>= \x -> io $ klog Magenta $ show x -- mapM_ sendJoin
        else io $ klogError $ "Login failed: " ++ (B.unpack $ fromJust $ getArg "e" pkt)
respond _ str = io $ print $ "Got the packet called " `B.append` str

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
                   LostClient -> io $ klogError "Lost client connection, DCing server"
                   ParseFailure -> io $ klogError "Malformed communication from server"
                   _ -> io $ klogError "Got the wrong exception"),
               Handler (\(e :: IOException) -> io $ klogError $ "server: " ++ show e)]