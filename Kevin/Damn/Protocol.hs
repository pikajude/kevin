module Kevin.Damn.Protocol (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Util.Entity
import Kevin.Util.Tablump
import Kevin.Damn.Packet
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub, delete, sortBy)
import Data.Ord (comparing)
import Kevin.Damn.Protocol.Send
import qualified Kevin.IRC.Protocol.Send as I
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.Time.Clock.POSIX (getPOSIXTime)

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = klog Blue "cleanup server"

listen :: KevinIO ()
listen = fix (\f -> flip catches errHandlers $ do
    k <- getK
    pkt <- io $ parsePacket <$> readServer k
    respond pkt (command pkt)
    f)

-- main responder
respond :: Packet -> T.Text -> KevinIO ()
respond _ "dAmnServer" = do
    set <- getsK settings
    let uname = getUsername set
        token = getAuthtoken set
    sendLogin uname token

respond pkt "login" = if okay pkt
    then do
        modifyK logIn
        getsK toJoin >>= mapM_ sendJoin
    else I.sendNotice $ "Login failed: " `T.append` getArg "e" pkt

respond pkt "join" = do
    roomname <- deformatRoom r
    if okay pkt
        then do
            modifyK (onJoining (r:))
            uname <- getsK (getUsername . settings)
            I.sendJoin uname roomname
        else I.sendNotice $ T.concat ["Couldn't join ", roomname, ": ", getArg "e" pkt]
    where
        r = fromJust $ parameter pkt

respond pkt "part" = do
    roomname <- deformatRoom $ fromJust $ parameter pkt
    if okay pkt
        then do
            uname <- getsK (getUsername . settings)
            modifyK (removeRoom roomname)
            I.sendPart uname roomname Nothing
        else I.sendNotice $ T.concat ["Couldn't part ", roomname, ": ", getArg "e" pkt]

respond pkt "property" = deformatRoom (fromJust $ parameter pkt) >>= \roomname ->
    case getArg "p" pkt of
    "privclasses" -> do
        let pcs = parsePrivclasses $ fromJust $ body pkt
        modifyK (onPrivclasses (setPrivclasses roomname pcs))
        
    "topic" -> do
        uname <- getsK (getUsername . settings)
        I.sendTopic uname roomname (getArg "by" pkt) (T.replace "\n" " - " $ tablumpDecode $ fromJust $ body pkt) (getArg "ts" pkt)
        
    "title" -> modifyK (onTitles (setTitle roomname (T.replace "\n" " - " $ tablumpDecode $ fromJust $ body pkt)))
    
    "members" -> do
        (pcs,(uname,j)) <- getsK (privclasses &&& getUsername . settings &&& joining)
        let members = map (mkUser roomname pcs . parsePacket) $ init $ splitOn "\n\n" $ fromJust (body pkt)
            pc = privclass $ head $ filter (\x -> username x == uname) members
        modifyK (onUsers (setUsers roomname members))
        when (fromJust (parameter pkt) `elem` j) $ do
            I.sendUserList uname (nub members) roomname
            I.sendWhoList uname (nub members) roomname
            I.sendSetUserMode uname roomname $ getPcLevel roomname pc pcs
            modifyK (onJoining (delete $ fromJust $ parameter pkt))
        
    "info" -> do
        us <- getsK (getUsername . settings)
        curtime <- io $ floor <$> getPOSIXTime
        let fixedPacket = parsePacket $ T.init $ T.replace "\n\nusericon" "\nusericon" $ readable pkt
            uname = T.drop 6 $ fromJust $ parameter pkt
            rn = getArg "realname" fixedPacket
            conns = map (\x -> (read (T.unpack $ getArg "online" x) :: Int, read (T.unpack $ getArg "idle" x) :: Int, map (T.drop 8) $ filter (not . T.null) $ T.splitOn "\n\n" $ fromJust $ body x)) $ fromJust $ (map (parsePacket . T.append "conn") . tail . T.splitOn "conn") <$> body fixedPacket
            allRooms = nub $ conns >>= (\(_,_,c) -> c)
            (onlinespan,idle) = head $ sortBy (comparing fst) $ map (\(a,b,_) -> (a,b)) conns
            signon = curtime - onlinespan
        I.sendWhoisReply us uname (entityDecode rn) allRooms idle signon
    
    q -> klogError $ "Unrecognized property " ++ T.unpack q

respond spk "recv" = deformatRoom (fromJust $ parameter spk) >>= \roomname ->
    case command pkt of
    "join" -> do
        let usname = fromJust $ parameter pkt
        (pcs,countUser) <- getsK (privclasses &&& numUsers roomname usname . users)
        let us = mkUser roomname pcs modifiedPkt
        modifyK (onUsers (addUser roomname us))
        if countUser == 0
            then do
                I.sendJoin usname roomname
                I.sendSetUserMode usname roomname $ getPcLevel roomname (getArg "pc" modifiedPkt) pcs
            else I.sendNoticeClone (username us) (succ countUser) roomname
    
    "part" -> do
        let uname = fromJust $ parameter pkt
        modifyK (onUsers (removeUser roomname uname))
        countUser <- getsK (numUsers roomname uname . users)
        if countUser < 1
            then I.sendPart uname roomname $ case getArg "r" pkt of { "" -> Nothing; x -> Just x }
            else I.sendNoticeUnclone uname countUser roomname
            
    "msg" -> do
        let uname = arg "from"
            msg   = fromJust (body pkt)
        un <- getsK (getUsername . settings)
        unless (un == uname) $ I.sendChanMsg uname roomname (entityDecode $ tablumpDecode msg)
    
    "action" -> do
        let uname = arg "from"
            msg   = fromJust (body pkt)
        un <- getsK (getUsername . settings)
        unless (un == uname) $ I.sendChanAction uname roomname (entityDecode $ tablumpDecode msg)
    
    "privchg" -> do
        (pcs,us) <- getsK (privclasses &&& users)
        let user = fromJust $ parameter pkt
            by = arg "by"
            oldPc = getPc roomname user us
            newPc = arg "pc"
            oldPcLevel = fmap (\p -> getPcLevel roomname p pcs) oldPc
            newPcLevel = getPcLevel roomname newPc pcs
        modifyK (setUserPrivclass roomname user newPc)
        I.sendRoomNotice roomname $ T.concat [user, " has been moved", maybe "" (T.append " from ") oldPc, " to ", newPc, " by ", by]
        I.sendChangeUserMode user roomname (fromMaybe 0 oldPcLevel) newPcLevel

    "kicked" -> do
        let uname = fromJust $ parameter pkt
        modifyK (onUsers (removeUserAll roomname uname))
        I.sendKick uname (arg "by") roomname $ case body pkt of {Just "" -> Nothing; x -> x}
            
    "admin" -> case fromJust $ parameter pkt of
        "create" -> I.sendRoomNotice roomname $ T.concat ["Privclass ", arg "name", " created by ", arg "by", " with: ", arg "privs"]
        "update" -> I.sendRoomNotice roomname $ T.concat ["Privclass ", arg "name", " updated by ", arg "by", " with: ", arg "privs"]
        "rename" -> I.sendRoomNotice roomname $ T.concat ["Privclass ", arg "prev", " renamed to ", arg "name", " by ", arg "by"]
        "move"   -> I.sendRoomNotice roomname $ T.concat [arg "n", " users in privclass ", arg "prev", " moved to ", arg "name", " by ", arg "by"]
        "remove" -> I.sendRoomNotice roomname $ T.concat ["Privclass", arg "name", " removed by ", arg "by"]
        "show"   -> mapM_ (I.sendRoomNotice roomname) $ T.splitOn "\n" $ fromJust $ body pkt
        "privclass" -> I.sendRoomNotice roomname $ "Admin error: " `T.append` arg "e"
        q -> klogError $ "Unknown admin packet type " ++ show q
    
    x -> klogError $ "Unknown packet type " ++ show x
    
    where
        pkt = fromJust $ subPacket spk
        modifiedPkt = parsePacket (T.replace "\n\npc" "\npc" (fromJust $ body spk))
        arg = flip getArg pkt

respond pkt "send" = I.sendNotice $ T.concat ["Send error: ", getArg "e" pkt]

respond _ "ping" = getK >>= \k -> io $ writeServer k ("pong\n\0" :: T.Text)

respond _ str = klog Yellow $ "Got the packet called " ++ T.unpack str


mkUser :: Chatroom -> PrivclassStore -> Packet -> User
mkUser room st p = User (fromJust $ parameter p)
                       (g "pc")
                       (getPcLevel room (g "pc") st)
                       (g "symbol")
                       (entityDecode $ g "realname")
                       (g "typename")
                       (g "gpc")
    where
        g = flip getArg p

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(_ :: KevinException) -> klogError "Malformed communication from server"),
               Handler (\(e :: IOException) -> klogError $ "server: " ++ show e)]