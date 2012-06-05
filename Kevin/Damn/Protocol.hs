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

respond pkt "join" = if okay pkt
    then do
        uname <- getsK (getUsername . settings)
        (I.sendJoin uname . deformatRoom . fromJust) $ parameter pkt
    else I.sendNotice $ "Join failed: " `T.append` getArg "e" pkt

respond pkt "property" = case getArg "p" pkt of
    "privclasses" -> do
        let pcs = parsePrivclasses $ fromJust $ body pkt
        modifyK (onPrivclasses (setPrivclasses roomname pcs))
        
    "topic" -> do
        uname <- getsK (getUsername . settings)
        I.sendTopic uname roomname (getArg "by" pkt) (T.replace "\n" " - " $ tablumpDecode $ fromJust $ body pkt) (getArg "ts" pkt)
        
    "title" -> modifyK (onTitles (setTitle roomname (fromJust (body pkt))))
    
    "members" -> do
        (pcs,uname) <- getsK (privclasses &&& getUsername . settings)
        let members = map (mkUser roomname pcs . parsePacket) $ init $ splitOn "\n\n" $ fromJust (body pkt)
            pc = privclass $ head $ filter (\x -> username x == uname) members
        modifyK (onUsers (setUsers roomname members))
        I.sendUserList uname (nub members) roomname
        I.sendWhoList uname (nub members) roomname
        I.sendSetUserMode uname roomname $ getPcLevel roomname pc pcs
        
    x | "login:" `T.isPrefixOf` x -> klog Blue "got user info"
    
    q -> klogError $ "Unrecognized property " ++ T.unpack q
    
    where roomname = (deformatRoom . fromJust . parameter) pkt

respond spk "recv" = case command pkt of
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
        let uname = getArg "from" pkt
            msg   = fromJust (body pkt)
        un <- getsK (getUsername . settings)
        unless (un == uname) $ I.sendChanMsg uname roomname (entityDecode $ tablumpDecode msg)
    
    "action" -> do
        let uname = getArg "from" pkt
            msg   = fromJust (body pkt)
        un <- getsK (getUsername . settings)
        unless (un == uname) $ I.sendChanAction uname roomname (entityDecode $ tablumpDecode msg)
    
    "privchg" -> do
        (pcs,us) <- getsK (privclasses &&& users)
        let user = fromJust $ parameter pkt
            by = getArg "by" pkt
            oldPc = fromJust $ getPc roomname user us
            newPc = getArg "pc" pkt
            oldPcLevel = getPcLevel roomname oldPc pcs
            newPcLevel = getPcLevel roomname newPc pcs
        modifyK (setUserPrivclass roomname user newPc)
        I.sendNotice $ T.concat [user, " has been moved from ", oldPc, " to ", newPc, " by ", by]
        I.sendChangeUserMode user roomname oldPcLevel newPcLevel
    
    "kicked" -> do
        let uname = fromJust $ parameter pkt
        modifyK (onUsers (removeUserAll roomname uname))
        I.sendKick uname (getArg "by" pkt) roomname $ case body pkt of {Just "" -> Nothing; x -> x}
            
    x -> klogError $ "Haven't yet handled " ++ T.unpack x
    
    where
        pkt = fromJust $ subPacket spk
        modifiedPkt = parsePacket (T.replace "\n\npc" "\npc" (fromJust $ body spk))
        roomname = (deformatRoom . fromJust . parameter) spk

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