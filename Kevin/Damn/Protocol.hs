module Kevin.Damn.Protocol (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import Kevin.Util.Entity
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
        
    x | "login:" `T.isPrefixOf` x -> klog Blue "got user info"
    
    q -> klogError $ "Unrecognized property " ++ T.unpack q
    
    where roomname = (deformatRoom . fromJust . parameter) pkt

respond spk "recv" = case command pkt of
    "join" -> do
        pcs <- getsK privclasses
        let us = mkUser roomname pcs modifiedPkt
        modifyK (onUsers (addUser roomname us))
        countUser <- getsK (numUsers roomname (username us) . users)
        if countUser == 1
            then I.sendJoin (fromJust (parameter pkt)) roomname
            else I.sendNoticeClone (username us) countUser roomname
    
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
        unless (un == uname) $ I.sendChanMsg uname roomname (entityDecode msg)
             
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
                       (fromJust $ getPcLevel room (g "pc") st)
                       (g "symbol")
                       (entityDecode $ g "realname")
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