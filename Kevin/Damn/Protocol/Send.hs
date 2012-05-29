module Kevin.Damn.Protocol.Send (
    sendPacket,
    formatRoom,
    deformatRoom,
    
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
import Kevin.Damn.Packet
import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import Data.Char (toLower)

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeServer k p

formatRoom :: B.ByteString -> KevinIO B.ByteString
formatRoom b = 
    case B.splitAt 1 b of
        ("#",s) -> return $ "chat:" `B.append` s
        ("&",s) -> do
            uname <- getsK (username . settings)
            return $ B.append "pchat:" $ B.intercalate ":" $ sort $ map (B.map toLower) [uname, s]
        r@(_,_) -> return $ "chat" `B.append` uncurry B.append r

deformatRoom :: B.ByteString -> B.ByteString
deformatRoom room = if "chat:" `B.isPrefixOf` room
    then '#' `B.cons` B.drop 5 room
    else '&' `B.cons` last (B.split ',' room)

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
           , args = [("agent","kevin " `B.append` VERSION)]
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