module Kevin.IRC.Protocol.Send (
    sendJoin,
    sendPart,
    sendSetUserMode,
    sendChangeUserMode,
    sendNotice,
	sendRoomNotice,
    sendChanMsg,
    sendChanAction,
    sendKick,
    sendTopic,
    sendChanMode,
    sendUserList,
    sendPong,
    sendNoticeClone,
    sendNoticeUnclone,
    sendWhoisReply
) where

import qualified Data.Text as T
import Kevin.Base

hostname :: T.Text
hostname = ":chat.deviantart.com"

getHost :: T.Text -> T.Text
getHost u = T.concat [":", u, "!", u, "@chat.deviantart.com"]

sendPacket :: T.Text -> KevinIO ()
sendPacket p = get_ >>= \k -> io . writeClient k $ T.append p "\r\n"

maybeBody :: Maybe T.Text -> T.Text
maybeBody = maybe "" (T.append " :")

type Str = T.Text
type Room = Str
type Username = Str

sendJoin :: Username -> Room -> KevinIO ()
sendPart :: Username -> Room -> Maybe Str -> KevinIO ()
sendSetUserMode :: Username -> Room -> Int -> KevinIO ()
sendChangeUserMode :: Username -> Room -> Int -> Int -> KevinIO ()
sendNotice :: Str -> KevinIO ()
sendRoomNotice :: Room -> Str -> KevinIO ()
sendChanMsg, sendChanAction :: Username -> Room -> Str -> KevinIO ()
sendKick :: Username -> Username -> Room -> Maybe Str -> KevinIO ()
sendTopic :: Username -> Room -> Username -> Str -> Str -> KevinIO ()
sendChanMode :: Username -> Room -> KevinIO ()
sendUserList :: Username -> [User] -> Room -> KevinIO ()
sendPong :: T.Text -> KevinIO ()
sendNoticeClone :: Username -> Int -> Room -> KevinIO ()
sendNoticeUnclone :: Username -> Int -> Room -> KevinIO ()
sendWhoisReply :: Username -> Username -> Username -> [Room] -> Int -> Int -> KevinIO ()

sendJoin us rm =
    sendPacket $ printf "%s JOIN :%s" [getHost us, rm]

sendPart us rm msg =
    sendPacket $ printf "%s PART %s%s" [getHost us, rm, maybeBody msg]

sendSetUserMode us rm m = unless (T.null mode) $
    sendPacket $ printf "%s MODE %s +%s %s" [hostname, rm, mode, us]
    where mode = levelToMode m

sendChangeUserMode us rm old new = unless (oldMode == newMode) $
    sendPacket $ printf "%s MODE %s %s" [hostname, rm, modesAndUser]
    where
        oldMode = levelToMode old
        newMode = levelToMode new
        modesAndUser = case (oldMode, newMode) of
            ("", "") -> T.concat ["-v ", us]
            ("", _) -> T.concat ["+", newMode, " ", us]
            (_, "") -> T.concat ["-", oldMode, " ", us]
            (a, b) -> T.concat ["-", a, "+", b, " ", us, " ", us]


sendNotice =
    sendPacket . printf "NOTICE AUTH :%s" . return

sendRoomNotice room n =
	sendPacket $ printf "%s NOTICE %s :%s" [hostname, room, n]

sendChanMsg sender room msg = mapM_ (\x ->
    sendPacket $ printf "%s PRIVMSG %s :%s" [getHost sender, room, x]
    ) . T.splitOn "\n" $ msg

sendChanAction sender room msg = mapM_ (\x ->
    sendPacket $ printf "%s PRIVMSG %s :\1ACTION %s\1" [getHost sender, room, x]
    ) . T.splitOn "\n" $ msg

sendKick kickee kicker room msg =
    sendPacket $ printf "%s KICK %s %s%s" [getHost kicker, room, kickee, maybeBody msg]

sendTopic us rm maker top startdate = do
    sendPacket $ printf "%s 332 %s %s :%s" [hostname, us, rm, top]
    sendPacket $ printf "%s 333 %s %s %s %s" [hostname, us, rm, maker, startdate]

sendChanMode us rm = do
    sendPacket $ printf "%s 324 %s %s +t" [hostname, us, rm]
    sendPacket $ printf "%s 329 %s %s 767529000" [hostname, us, rm]

sendUserList us uss rm = do
    mapM_ (\nms ->
        sendPacket $ printf "%s 353 %s = %s :%s" [hostname, us, rm, T.unwords nms]) chunkedNames
    sendPacket $ printf "%s 366 %s %s :End of /NAMES list." [hostname, us, rm]
    where
        names = map (\u -> T.concat [levelToSym $ privclassLevel u, username u]) uss
        chunkedNames = reverse . map reverse . subchunk' 432 names $ [[]]
        subchunk' n = fix (\f x y -> let hy = head y; hx = head x; ty = tail y; tx = tail x in if null x
            then y
            else f tx $ if sum (map T.length hy) + T.length hx <= n
                then (hx:hy):ty
                else [hx]:y)

sendPong p =
    sendPacket $ printf "%s PONG chat.deviantart.com :%s" [hostname, p]

sendNoticeClone uname i rm =
    sendPacket $ printf "%s NOTICE %s :%s has joined again (now joined %s times)" [hostname, rm, uname, T.pack $ show i]

sendNoticeUnclone uname i rm =
    sendPacket $ printf "%s NOTICE %s :%s has parted (now joined %s)" [hostname, rm, uname, times]
    where
        times | i == 1    = "once"
              | otherwise = T.pack (show i) `T.append` " times"

sendWhoisReply me us rn rooms idle signon = do
    sendPacket $ printf "%s 311 %s %s %s chat.deviantart.com * :%s" [hostname, me, us, us, rn]
    sendPacket $ printf "%s 307 %s %s :is a registered nick" [hostname, me, us]
    sendPacket $ printf "%s 319 %s %s :%s" [hostname, me, us, T.intercalate " " . map (T.cons '#') $ rooms]
    sendPacket $ printf "%s 312 %s %s chat.deviantart.com :dAmn" [hostname, me, us]
    sendPacket $ printf "%s 317 %s %s %s %s :seconds idle, signon time" [hostname, me, us, T.pack $ show idle, T.pack $ show signon]
    sendPacket $ printf "%s 318 %s %s :End of /WHOIS list." [hostname, me, us]

levelToSym :: Int -> T.Text
levelToSym x | x > 0  && x <= 35 = ""
             | x > 35 && x <= 70 = "+"
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
