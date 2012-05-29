module Kevin.Protocol.IRC.Send (
    sendJoin,
    sendPart,
    sendChanMsg,
    sendPrivMsg,
    sendKick,
    sendPromote
) where
    
import Kevin.Base
import Kevin.Packet.IRC
import qualified Data.ByteString.Char8 as B

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeClient k p

type Room = B.ByteString
type User = Room
type Str = Room

sendJoin, sendPart :: User -> Room -> KevinIO ()
sendChanMsg :: User -> Room -> Str -> KevinIO ()
sendPrivMsg :: User -> Str -> KevinIO ()
sendKick :: User -> Room -> Maybe Str -> KevinIO ()
sendPromote :: User -> Room -> Privclass -> Privclass -> KevinIO ()

sendJoin us rm = sendPacket
    Packet { prefix = Just $ B.concat [us, "!", us, "@chat.deviantart.com"]
           , command = "JOIN"
           , params = [rm]
           }
sendPart = undefined
sendChanMsg = undefined
sendPrivMsg = undefined
sendKick = undefined
sendPromote = undefined