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
type Username = Str

sendJoin, sendPart :: Username -> Room -> KevinIO ()
sendChanMsg :: Username -> Room -> Str -> KevinIO ()
sendPrivMsg :: Username -> Str -> KevinIO ()
sendKick :: Username -> Room -> Maybe Str -> KevinIO ()
sendPromote :: Username -> Room -> Privclass -> Privclass -> KevinIO ()
sendTopic :: Username -> Room -> Username -> Str -> Str -> KevinIO ()
sendUserList :: Username -> [(Username,Int)] -> Room -> KevinIO ()

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
sendUserList us uss rm = sendPacket
    Packet { prefix = hostname
           , command = "353"
           , params = [us, "=", rm, B.intercalate " " (map (\(name,level) -> B.concat [levelToSym level, name]) uss)]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "366"
           , params = [us, rm, "End of NAMES list."]
           }

levelToSym :: Int -> B.ByteString
levelToSym x | x > 0  && x <= 10 = ""
             | x > 10 && x <= 70 = "+"
             | x > 70 && x <  99 = "@"
             | x == 99           = "~"
             | otherwise         = ""

levelToMode :: Int -> B.ByteString
levelToMode x = case levelToSym x of
   "" -> ""
   "+" -> "v"
   "@" -> "o"
   "~" -> "q"
   _ -> error "levelToSym, what are you doing"