module Kevin.Protocol.Damn (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import Kevin.Packet.Damn
import qualified Data.ByteString.Char8 as B

initialize :: KevinIO ()
initialize = sendPacket
    Packet { command = "dAmnClient"
           , parameter = Just "0.3"
           , args = [("agent","kevin 0.1")]
           , body = Nothing
           }

cleanup :: KevinIO ()
cleanup = io $ klog Blue "cleanup server"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- get
    pkt <- io $ fmap parsePacket $ readServer k
    respond pkt (command pkt)
    listen

sendPacket :: Packet -> KevinIO ()
sendPacket p = get >>= \k -> io $ writeServer k p

respond :: Packet -> B.ByteString -> KevinIO ()
respond _ "dAmnServer" = do
    set <- gets settings
    let uname = username set
        token = authtoken set
    sendPacket Packet { command = "login"
                      , parameter = Just uname
                      , args = [("pk",token)]
                      , body = Nothing
                      }
respond _ str = io $ print $ "Got the packet called " `B.append` str

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
                   LostClient -> io $ klogError "Lost client connection, DCing server"
                   ParseFailure -> io $ klogError "Malformed communication from server"
                   _ -> io $ klogError "Got the wrong exception"),
               Handler (\(e :: IOException) -> io $ klogError $ "server: " ++ show e)]