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
    sendGetProperty,
    sendWhois,
    sendSet,
    sendAdmin,
    sendKill
) where

import Kevin.Base
import Kevin.Damn.Packet
import qualified Data.Text as T
import Data.List (sort)
import Data.Char (toLower)

sendPacket :: Packet -> KevinIO ()
sendPacket p = getK >>= \k -> io $ writeServer k p

formatRoom :: T.Text -> KevinIO T.Text
formatRoom b = 
    case T.splitAt 1 b of
        ("#",s) -> return $ "chat:" `T.append` s
        ("&",s) -> do
            uname <- getsK (getUsername . settings)
            return $ T.append "pchat:" $ T.intercalate ":" $ sort $ map (T.map toLower) [uname, s]
        r -> return $ "chat" `T.append` uncurry T.append r

deformatRoom :: T.Text -> T.Text
deformatRoom room = if "chat:" `T.isPrefixOf` room
    then '#' `T.cons` T.drop 5 room
    else '&' `T.cons` last (T.splitOn "," room)

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
sendGetProperty :: Room -> Str -> KevinIO ()
sendWhois :: Username -> KevinIO ()
sendSet :: Room -> Str -> Str -> KevinIO ()
sendAdmin :: Room -> Str -> KevinIO ()
sendKill :: Username -> Str -> KevinIO ()

sendHandshake = sendPacket
    Packet { command = "dAmnClient"
           , parameter = Just "0.3"
           , args = [("agent","kevin " `T.append` VERSION)]
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

sendPart room = do
    roomname <- formatRoom room
    sendPacket Packet { command = "part"
                      , parameter = Just roomname
                      , args = []
                      , body = Nothing
                      }

sendMsg room msg = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["msg main\n\n", msg]
                      }

sendAction room msg = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["action main\n\n", msg]
                      }

sendNpMsg = undefined

sendPromote room us pc = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["promote ", us, maybe "" (T.append "\n\n") pc]
                      }

sendDemote room us pc = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["demote ", us, maybe "" (T.append "\n\n") pc]
                      }

sendBan room us = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["ban ", us, "\n"]
                      }

sendUnban room us = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["unban ", us, "\n"]
                      }

sendKick room us reason = do
    roomname <- formatRoom room
    sendPacket Packet { command = "kick"
                      , parameter = Just roomname
                      , args = [("u",us)]
                      , body = reason
                      }

sendGetProperty = undefined

sendWhois us = sendPacket
    Packet { command = "get"
           , parameter = Just $ "login:" `T.append` us
           , args = [("p","info")]
           , body = Nothing
           }

sendSet = undefined
sendAdmin = undefined
sendKill = undefined