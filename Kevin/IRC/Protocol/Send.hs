module Kevin.IRC.Protocol.Send (
    sendJoin,
    sendPart,
    sendChanMsg,
    sendPrivMsg,
    sendKick,
    sendPromote,
    sendTopic,
    sendChanMode,
    sendUserList,
    sendWhoList,
    sendPong
) where
    
import Kevin.Base
import Kevin.IRC.Packet
import qualified Data.Text as T

hostname :: Maybe T.Text
hostname = Just "chat.deviantart.com"

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeClient k p

type Str = T.Text
type Room = Str
type Username = Str

sendJoin, sendPart :: Username -> Room -> KevinIO ()
sendChanMsg :: Username -> Room -> Str -> KevinIO ()
sendPrivMsg :: Username -> Str -> KevinIO ()
sendKick :: Username -> Room -> Maybe Str -> KevinIO ()
sendPromote :: Username -> Room -> Privclass -> Privclass -> KevinIO ()
sendTopic :: Username -> Room -> Username -> Str -> Str -> KevinIO ()
sendChanMode :: Username -> Room -> KevinIO ()
sendUserList :: Username -> [User] -> Room -> KevinIO ()
sendWhoList :: Username -> [User] -> Room -> KevinIO ()
sendPong :: T.Text -> KevinIO ()

sendJoin us rm = sendPacket
    Packet { prefix = Just $ T.concat [us, "!", us, "@chat.deviantart.com"]
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
           , params = [us, rm, top `T.snoc` ' ']
           } >> sendPacket
    Packet { prefix = hostname
           , command = "333"
           , params = [us, rm, maker, startdate]
           }

sendChanMode us rm = sendPacket
    Packet { prefix = hostname
           , command = "324"
           , params = [us, rm, "+t"]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "329"
           , params = [us, rm, "767529000"]
           }

sendUserList us uss rm = sendPacket
    Packet { prefix = hostname
           , command = "353"
           , params = [us, "=", rm, T.intercalate " " (map (\u -> T.concat [levelToSym $ privclassLevel u, username u]) uss)]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "366"
           , params = [us, rm, "End of NAMES list."]
           }

sendWhoList us uss rm = mapM_ (sendPacket . (\u ->
    Packet { prefix = hostname
           , command = "352"
           , params = [us, rm, username u, "chat.deviantart.com", "chat.deviantart.com", username u, "Hr" `T.append` symbol u, "0 " `T.append` realname u]
           })) uss >> sendPacket
    Packet { prefix = hostname
           , command = "315"
           , params = [us, rm, "End of WHO list."]
           }

sendPong p = sendPacket
    Packet { prefix = hostname
           , command = "PONG"
           , params = ["chat.deviantart.com", p `T.snoc` ' ']
           }

levelToSym :: Int -> T.Text
levelToSym x | x > 0  && x <= 10 = ""
             | x > 10 && x <= 70 = "+"
             | x > 70 && x <  99 = "@"
             | x == 99           = "~"
             | otherwise         = ""

levelToMode :: Int -> T.Text
levelToMode x = case levelToSym x of
   "" -> ""
   "+" -> "v"
   "@" -> "o"
   "~" -> "q"
   _ -> error "levelToSym, what are you doing"