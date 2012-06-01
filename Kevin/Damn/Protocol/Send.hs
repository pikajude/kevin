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
    sendDisconnect,
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
sendJoin :: Room -> KevinIO ()
sendPart :: Room -> Maybe Str -> KevinIO ()
sendMsg, sendAction, sendNpMsg :: Room -> Str -> KevinIO ()
sendPromote, sendDemote :: Room -> Username -> Pc -> KevinIO ()
sendBan, sendUnban :: Room -> Username -> KevinIO ()
sendKick :: Room -> Username -> Str -> KevinIO ()
sendGetProperty :: Room -> Str -> KevinIO ()
sendWhois :: Username -> KevinIO ()
sendSet :: Room -> Str -> Str -> KevinIO ()
sendAdmin :: Room -> Str -> KevinIO ()
sendDisconnect :: KevinIO ()
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

sendPart = undefined

sendMsg room msg = do
    roomname <- formatRoom room
    sendPacket Packet { command = "send"
                      , parameter = Just roomname
                      , args = []
                      , body = Just $ T.concat ["msg main\n\n", msg]
                      }

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