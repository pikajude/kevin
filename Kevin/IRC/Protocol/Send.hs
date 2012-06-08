module Kevin.IRC.Protocol.Send (
    sendJoin,
    sendPart,
    sendSetUserMode,
    sendChangeUserMode,
    sendNotice,
    sendChanMsg,
    sendChanAction,
    sendPrivMsg,
    sendKick,
    sendTopic,
    sendChanMode,
    sendUserList,
    sendWhoList,
    sendPong,
    sendNoticeClone,
    sendNoticeUnclone,
    sendWhoisReply
) where
    
import Kevin.Base
import Kevin.IRC.Packet
import qualified Data.Text as T
import Data.Maybe
import Control.Applicative ((<$>))

fixColon ∷ T.Text → T.Text
fixColon str = if " " `T.isInfixOf` str then str else ':' `T.cons` str

hostname ∷ Maybe T.Text
hostname = Just "chat.deviantart.com"

getHost ∷ T.Text → Maybe T.Text
getHost u = Just $ T.concat [u, "!", u, "@chat.deviantart.com"]

sendPacket ∷ Packet → KevinIO ()
sendPacket p = getK >>= \k → io $ writeClient k p

asAction ∷ T.Text → T.Text
asAction x = T.concat ["\1ACTION ", x, "\1"]

type Str = T.Text
type Room = Str
type Username = Str

sendJoin ∷ Username → Room → KevinIO ()
sendPart ∷ Username → Room → Maybe Str → KevinIO ()
sendSetUserMode ∷ Username → Room → Int → KevinIO ()
sendChangeUserMode ∷ Username → Room → Int → Int → KevinIO ()
sendNotice ∷ Str → KevinIO ()
sendChanMsg, sendChanAction ∷ Username → Room → Str → KevinIO ()
sendPrivMsg ∷ Username → Str → KevinIO ()
sendKick ∷ Username → Username → Room → Maybe Str → KevinIO ()
sendTopic ∷ Username → Room → Username → Str → Str → KevinIO ()
sendChanMode ∷ Username → Room → KevinIO ()
sendUserList ∷ Username → [User] → Room → KevinIO ()
sendWhoList ∷ Username → [User] → Room → KevinIO ()
sendPong ∷ T.Text → KevinIO ()
sendNoticeClone ∷ Username → Int → Room → KevinIO ()
sendNoticeUnclone ∷ Username → Int → Room → KevinIO ()
sendWhoisReply ∷ Username → Username → Username → [Room] → Int → Int → KevinIO ()

sendJoin us rm = sendPacket
    Packet { prefix = getHost us
           , command = "JOIN"
           , params = [rm]
           }

sendPart us rm msg = sendPacket
    Packet { prefix = getHost us
           , command = "PART"
           , params = rm:(fixColon <$> maybeToList msg)
           }

sendSetUserMode us rm m = unless (T.null mode) $ sendPacket
    Packet { prefix = hostname
           , command = "MODE"
           , params = [rm, '+' `T.cons` mode, us]
           }
    where
        mode = levelToMode m

sendChangeUserMode us rm old new = unless (oldMode == newMode) $ sendPacket
    Packet { prefix = hostname
           , command = "MODE"
           , params = [rm, T.concat ["-", oldMode, "+", newMode], us]
           }
    where
        oldMode = levelToMode old
        newMode = levelToMode new

sendNotice str = sendPacket
    Packet { prefix = Nothing
           , command = "NOTICE"
           , params = ["AUTH", fixColon str]
           }

sendChanMsg sender room msg = mapM_ (\x → sendPacket
    Packet { prefix = getHost sender
           , command = "PRIVMSG"
           , params = [room, fixColon x]
           }) $ T.splitOn "\n" msg

sendChanAction sender room msg = mapM_ (\x → sendPacket
    Packet { prefix = getHost sender
           , command = "PRIVMSG"
           , params = [room, asAction x]
           }) $ T.splitOn "\n" msg

sendPrivMsg = undefined

sendKick kickee kicker room msg = sendPacket
    Packet { prefix = getHost kicker
           , command = "KICK"
           , params = [room, kickee] ++ maybeToList (fixColon <$> msg)
           }

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

sendUserList us uss rm = mapM_ (\nms → sendPacket
    Packet { prefix = hostname
           , command = "353"
           , params = [us, "=", rm, T.intercalate " " nms]
           }) chunkedNames >> sendPacket
    Packet { prefix = hostname
           , command = "366"
           , params = [us, rm, "End of NAMES list."]
           }
    where
        names = map (\u → T.concat [levelToSym $ privclassLevel u, username u]) uss
        chunkedNames = reverse $ map reverse $ subchunk' 432 names [[]]
        subchunk' n = fix (\f x y → let hy = head y; hx = head x; ty = tail y; tx = tail x in if null x
            then y
            else if sum (map T.length hy) + T.length hx <= n
                then f tx ((hx:hy):ty)
                else f tx ([hx]:y))

sendWhoList us uss rm = mapM_ (sendPacket . (\u →
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

sendWhoisReply me us rn rooms idle signon = sendPacket
    Packet { prefix = hostname
           , command = "311"
           , params = [me, us, us, "chat.deviantart.com", "*", fixColon rn]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "307"
           , params = [me, us, "is a registered nick"]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "319"
           , params = [me, us, fixColon $ T.intercalate " " $ map (T.cons '#') rooms]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "312"
           , params = [me, us, "chat.deviantart.com", ":dAmn"]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "317"
           , params = [me, us, T.pack $ show idle, T.pack $ show signon, "seconds idle, signon time"]
           } >> sendPacket
    Packet { prefix = hostname
           , command = "318"
           , params = [me, us, "End of /WHOIS list."]
           }

levelToSym ∷ Int → T.Text
levelToSym x | x > 0  && x <= 35 = ""
             | x > 35 && x <= 70 = "+"
             | x > 70 && x <  99 = "@"
             | x == 99           = "~"
             | otherwise         = ""

levelToMode ∷ Int → T.Text
levelToMode x = case levelToSym x of
   "" → ""
   "+" → "v"
   "@" → "o"
   "~" → "q"
   _ → error "levelToSym, what are you doing"
