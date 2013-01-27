module Kevin.Damn.Protocol.Send (
    sendPacket,
    formatRoom,
    deformatRoom,

    sendHandshake,
    sendLogin,
    sendJoin,
    sendPart,
    sendMsg,
    sendAction,
    sendNpMsg,
    sendPromote,
    sendDemote,
    sendBan,
    sendUnban,
    sendKick,
    sendGet,
    sendWhois,
    sendSet,
    sendAdmin,
    sendKill
) where

import Data.Char (toLower)
import Data.List (sort)
import Data.Monoid
import qualified Data.Text as T
import Kevin.Base

maybeBody :: Maybe T.Text -> T.Text
maybeBody = maybe "" (T.append "\n\n")

sendPacket :: T.Text -> KevinIO ()
sendPacket p = get_ >>= \k -> io . writeServer k . T.snoc p $ '\0'

formatRoom :: T.Text -> KevinIO T.Text
formatRoom b =
    case T.splitAt 1 b of
        ("#",s) -> return $ "chat:" <> s
        ("&",s) -> do
            uname <- use_ name
            return . T.append "pchat:" . T.intercalate ":"
                   . sort . map (T.map toLower) $ [uname, s]
        r -> return $ "chat" <> uncurry T.append r

deformatRoom :: T.Text -> KevinIO T.Text
deformatRoom room = if "chat:" `T.isPrefixOf` room
    then return $ '#' `T.cons` T.drop 5 room
    else do
        uname <- use_ name
        return $ '&' `T.cons` head (filter (/= uname) . T.splitOn ":" . T.drop 6 $ room)

type Str = T.Text -- just make it shorter
type Room = Str
type Username = Str
type Pc = Str

-- * Communication to the server
sendHandshake :: KevinIO ()
sendLogin :: Username -> Str -> KevinIO ()
sendJoin, sendPart :: Room -> KevinIO ()
sendMsg, sendAction, sendNpMsg :: Room -> Str -> KevinIO ()
sendPromote, sendDemote :: Room -> Username -> Maybe Pc -> KevinIO ()
sendBan, sendUnban :: Room -> Username -> KevinIO ()
sendKick :: Room -> Username -> Maybe Str -> KevinIO ()
sendGet :: Room -> Str -> KevinIO ()
sendWhois :: Username -> KevinIO ()
sendSet :: Room -> Str -> Str -> KevinIO ()
sendAdmin :: Room -> Str -> KevinIO ()
sendKill :: Username -> Str -> KevinIO ()

sendHandshake = sendPacket $ printf "dAmnClient 0.3\nagent=kevin%s\n" [VERSION]

sendLogin u token = sendPacket $ printf "login %s\npk=%s\n" [u, token]

sendJoin room = do
    roomname <- formatRoom room
    sendPacket $ printf "join %s\n" [roomname]

sendPart room = do
    roomname <- formatRoom room
    sendPacket $ printf "part %s\n" [roomname]

sendMsg room msg = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\nmsg main\n\n%s" [roomname, msg]

sendAction room msg = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\naction main\n\n%s" [roomname, msg]

sendNpMsg = undefined

sendPromote room us pc = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\npromote %s%s" [roomname, us, maybeBody pc]

sendDemote room us pc = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\ndemote %s%s" [roomname, us, maybeBody pc]

sendBan room us = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\nban %s\n\n" [roomname, us]

sendUnban room us = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\nunban %s\n\n" [roomname, us]

sendKick room us reason = do
    roomname <- formatRoom room
    sendPacket $ printf "kick %s\nu=%s%s\n" [roomname, us, maybeBody reason]

sendGet room prop = do
	guard $ prop `elem` ["title", "topic", "privclasses", "members"]
	roomname <- formatRoom room
	sendPacket $ printf "get %s\np=%s\n" [roomname, prop]

sendWhois us = sendPacket $ printf "get login:%s\np=info\n" [us]

sendSet room prop val = do
	guard (prop == "topic" || prop == "title")
	roomname <- formatRoom room
	sendPacket $ printf "set %s\np=%s\n\n%s\n" [roomname, prop, val]

sendAdmin room cmd = do
    roomname <- formatRoom room
    sendPacket $ printf "send %s\n\nadmin\n\n%s" [roomname, cmd]

sendKill = undefined
