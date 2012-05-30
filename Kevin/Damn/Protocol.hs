module Kevin.Damn.Protocol (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Damn.Packet
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.List (nub)
import Kevin.Damn.Protocol.Send
import qualified Kevin.IRC.Protocol.Send as I
import Control.Arrow ((&&&))

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = klog Blue "cleanup server"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- getK
    pkt <- io $ fmap parsePacket $ readServer k
    respond pkt (command pkt)
    listen

-- main responder
respond :: Packet -> B.ByteString -> KevinIO ()
respond _ "dAmnServer" = do
    set <- getsK settings
    let uname = getUsername set
        token = getAuthtoken set
    sendLogin uname token

respond pkt "login" = do
    uname <- getsK (getUsername . settings)
    if okay pkt
        then do
            klog Green $ "Logged in as " ++ B.unpack uname
            modifyK logIn
            getsK toJoin >>= mapM_ sendJoin
        else klogError $ "Login failed: " ++ B.unpack (getArg "e" pkt)

respond pkt "join" = if okay pkt
    then do
        uname <- getsK (getUsername . settings)
        (I.sendJoin uname . deformatRoom . fromJust) $ parameter pkt
    else klogError $ "Join failed: " ++ B.unpack (getArg "e" pkt)

respond pkt "property" = case getArg "p" pkt of
    "privclasses" -> do
        let pcs = parsePrivclasses $ fromJust $ body pkt
        modifyK (onPrivclasses (foldr ((.) . setPrivclass roomname) id pcs))
    "topic" -> do
        uname <- getsK (getUsername . settings)
        I.sendTopic uname roomname (getArg "by" pkt) (fromJust (body pkt)) (getArg "ts" pkt)
    "title" -> modifyK (onTitles (setTitle roomname (fromJust (body pkt))))
    "members" -> do
        (pcs,uname) <- getsK (privclasses &&& getUsername . settings)
        let members = map (mkUser roomname pcs . parsePacket) $ init $ splitOn "\n\n" $ fromJust (body pkt)
        modifyK (onUsers (setUsers roomname members))
        I.sendUserList uname (nub members) roomname
        I.sendWhoList uname (nub members) roomname
    x | "login:" `B.isPrefixOf` x -> klog Blue "got user info"
    q -> klogError $ "Unrecognized property " ++ B.unpack q
    where
        roomname = (deformatRoom . fromJust . parameter) pkt

respond _ str = io $ print $ "Got the packet called " `B.append` str

mkUser :: Chatroom -> PrivclassStore -> Packet -> User
mkUser room st p = User (fromJust $ parameter p)
                       (g "pc")
                       (fromJust $ getPcLevel room (g "pc") st)
                       (g "symbol")
                       (g "realname")
                       (g "typename")
                       (g "gpc")
    where
        g = flip getArg p


errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
                   LostClient -> klogError "Lost client connection, DCing server"
                   ParseFailure -> klogError "Malformed communication from server"
                   _ -> klogError "Got the wrong exception"),
               Handler (\(e :: IOException) -> klogError $ "server: " ++ show e)]