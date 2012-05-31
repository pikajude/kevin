module Kevin.IRC.Protocol.Send (
    sendJoin,
    sendPart,
    sendNotice,
    sendChanMsg,
    sendPrivMsg,
    sendKick,
    sendPromote,
    sendTopic,
    sendChanMode,
    sendUserList,
    sendWhoList,
    sendPong,
    sendNoticeClone,
    sendNoticeUnclone
) where
    
import Kevin.Base
import Kevin.IRC.Packet
import qualified Data.Text as T
import Data.Maybe

fixColon :: T.Text -> T.Text
fixColon str = if " " `T.isInfixOf` str then str else ':' `T.cons` str

hostname :: Maybe T.Text
hostname = Just "chat.deviantart.com"

getHost :: T.Text -> Maybe T.Text
getHost u = Just $ T.concat [u, "!", u, "@chat.deviantart.com"]

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeClient k p

type Str = T.Text
type Room = Str
type Username = Str

sendJoin :: Username -> Room -> KevinIO ()
sendPart :: Username -> Room -> Maybe Str -> KevinIO ()
sendNotice :: Str -> KevinIO ()
sendChanMsg :: Username -> Room -> Str -> KevinIO ()
sendPrivMsg :: Username -> Str -> KevinIO ()
sendKick :: Username -> Room -> Maybe Str -> KevinIO ()
sendPromote :: Username -> Room -> Privclass -> Privclass -> KevinIO ()
sendTopic :: Username -> Room -> Username -> Str -> Str -> KevinIO ()
sendChanMode :: Username -> Room -> KevinIO ()
sendUserList :: Username -> [User] -> Room -> KevinIO ()
sendWhoList :: Username -> [User] -> Room -> KevinIO ()
sendPong :: T.Text -> KevinIO ()
sendNoticeClone :: Username -> Int -> Room -> KevinIO ()
sendNoticeUnclone :: Username -> Int -> Room -> KevinIO ()

sendJoin us rm = sendPacket
    Packet { prefix = getHost us
           , command = "JOIN"
           , params = [rm]
           }

sendPart us rm msg = sendPacket
    Packet { prefix = getHost us
           , command = "PART"
           , params = rm:fmap fixColon (maybeToList msg)
           }

sendNotice str = sendPacket
    Packet { prefix = Nothing
           , command = "NOTICE"
           , params = ["AUTH", fixColon str]
           }

sendChanMsg sender room msg = sendPacket
    Packet { prefix = getHost sender
           , command = "PRIVMSG"
           , params = [room, fixColon msg]}

sendPrivMsg = undefined
sendKick = undefined
sendPromote = undefined

sendTopic us rm maker top startdate = sendPacket
    Packet { prefix = hostname
           , command = "332"
           , params = [us, rm, fixColon top]
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
           , params = ["chat.deviantart.com", fixColon p]
           }

sendNoticeClone uname i rm = sendPacket
    Packet { prefix = hostname
           , command = "NOTICE"
           , params = [rm, T.concat [uname, " has joined again (now joined ", T.pack $ show i, " times)"]]
           }

sendNoticeUnclone uname i rm = sendPacket
    Packet { prefix = hostname
           , command = "NOTICE"
           , params = [rm, T.concat [uname, " has parted (now joined ", times, ")"]]
           }
    where
        times | i == 1    = "once"
              | otherwise = T.pack (show i) `T.append` " times"

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