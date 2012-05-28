module Kevin.Protocol.Damn (
    initialize,
    cleanup,
    listen,
    errHandlers,
    
    -- * Packets!
    sendHandshake,
    sendLogin,
    sendJoin,
    sendPart,
    sendPong,
    sendMsg,
    sendAction,
    sendNpMsg,
    sendPromote,
    sendDemote,
    sendBan,
    sendUnban,
    sendKick,
    sendGetProperty,
    sendWhois,
    sendSet,
    sendAdmin,
    sendDisconnect,
    sendKill
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import Kevin.Packet.Damn
import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import Data.Char (toLower)

initialize :: KevinIO ()
initialize = sendHandshake

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
    sendLogin uname token
respond _ str = io $ print $ "Got the packet called " `B.append` str

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
                   LostClient -> io $ klogError "Lost client connection, DCing server"
                   ParseFailure -> io $ klogError "Malformed communication from server"
                   _ -> io $ klogError "Got the wrong exception"),
               Handler (\(e :: IOException) -> io $ klogError $ "server: " ++ show e)]

formatRoom :: B.ByteString -> KevinIO B.ByteString
formatRoom b = do
    uname <- gets (username . settings)
    if B.head b == '#'
        then return $ "chat:" `B.append` b
        else return $ B.append "pchat:" $ B.intercalate ":" $ sort $ map (B.map toLower) [uname, b]

type Str = B.ByteString -- just make it shorter
type Room = Str
type User = Str
type Pc = Str

-- * Communication to the server
sendHandshake :: KevinIO ()
sendLogin :: User -> Str -> KevinIO ()
sendJoin :: Room -> KevinIO ()
sendPart :: Room -> Maybe Str -> KevinIO ()
sendPong :: KevinIO ()
sendMsg, sendAction, sendNpMsg :: Room -> Str -> KevinIO ()
sendPromote, sendDemote :: Room -> User -> Pc -> KevinIO ()
sendBan, sendUnban :: Room -> User -> KevinIO ()
sendKick :: Room -> User -> Str -> KevinIO ()
sendGetProperty :: Room -> Str -> KevinIO ()
sendWhois :: User -> KevinIO ()
sendSet :: Room -> Str -> Str -> KevinIO ()
sendAdmin :: Room -> Str -> KevinIO ()
sendDisconnect :: KevinIO ()
sendKill :: User -> Str -> KevinIO ()

sendHandshake = sendPacket
    Packet { command = "dAmnClient"
           , parameter = Just "0.3"
           , args = [("agent","kevin 0.1")]
           , body = Nothing
           }

sendLogin u token = sendPacket
    Packet { command = "login"
           , parameter = Just u
           , args = [("pk",token)]
           , body = Nothing
           }

sendJoin room = do
    roomname <- formatRoom room
    sendPacket Packet { command = "join"
                      , parameter = Just roomname
                      , args = []
                      , body = Nothing
                      }

sendPart = undefined
sendPong = undefined
sendMsg = undefined
sendAction = undefined
sendNpMsg = undefined
sendPromote = undefined
sendDemote = undefined
sendBan = undefined
sendUnban = undefined
sendKick = undefined
sendGetProperty = undefined
sendWhois = undefined
sendSet = undefined
sendAdmin = undefined
sendDisconnect = undefined
sendKill = undefined