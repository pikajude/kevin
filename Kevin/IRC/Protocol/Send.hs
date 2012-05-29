module Kevin.IRC.Protocol.Send (
    sendJoin,
    sendPart,
    sendChanMsg,
    sendPrivMsg,
    sendKick,
    sendPromote,
    sendTopic,
    sendUserList
) where
    
import Kevin.Base
import Kevin.IRC.Packet
import qualified Data.ByteString.Char8 as B

hostname :: Maybe B.ByteString
hostname = Just "chat.deviantart.com"

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeClient k p

type Str = B.ByteString
type Room = Str
type User = Str

sendJoin, sendPart :: User -> Room -> KevinIO ()
sendChanMsg :: User -> Room -> Str -> KevinIO ()
sendPrivMsg :: User -> Str -> KevinIO ()
sendKick :: User -> Room -> Maybe Str -> KevinIO ()
sendPromote :: User -> Room -> Privclass -> Privclass -> KevinIO ()
sendTopic :: User -> Room -> User -> Str -> Str -> KevinIO ()
sendUserList :: [(User,Str)] -> Room -> KevinIO ()

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
sendTopic us rm maker top startdate = sendPacket
    Packet { prefix = hostname
           , command = "332"
           , params = [us, rm, top `B.snoc` ' ']
           } >> sendPacket
    Packet { prefix = hostname
           , command = "333"
           , params = [us, rm, maker, startdate]
           }
sendUserList = undefined