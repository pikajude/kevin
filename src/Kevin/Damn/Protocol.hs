{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevin.Damn.Protocol (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import           Control.Applicative          ((<$>), (<$))
import           Control.Concurrent
import           Control.Exception as E       (throw)
import           Control.Exception.Lens
import           Data.List                    (delete, nub, minimumBy)
import           Data.Maybe                   (fromMaybe, isNothing)
import           Data.Monoid
import           Data.Ord                     (comparing)
import qualified Data.Text as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Kevin.Base
import           Kevin.Damn.Protocol.Send
import qualified Kevin.IRC.Protocol.Send as I
import           Kevin.Util.Entity
import           Kevin.Util.Logger
import           Kevin.Util.Tablump
import           Text.Damn.Packet hiding      (parse)
import qualified Text.Damn.Packet as D        (parse)
import           Text.Trifecta.Result

okay :: Packet -> Bool
okay p = let e = pktArgs p ^. at "e" in isNothing e || e == Just "ok"

notNull_ :: Prism' T.Text T.Text
notNull_ = prism' id $ toMaybe (not . T.null)
    where toMaybe f x = x <$ guard (f x)

parsePrivclasses :: T.Text -> [Privclass]
parsePrivclasses = map (liftM2 (,) (!! 1) (read . T.unpack . head) . T.splitOn ":")
                 . filter (not . T.null)
                 . T.splitOn "\n"

parse :: T.Text -> Packet
parse m = case D.parse $ encodeUtf8 (fromMaybe m $ T.stripSuffix "\n" m) of
              Failure err_ -> E.throw $ ServerParseFailure (show err_) m
              Success x -> x

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = do
    klog Blue "cleanup server"
    k <- gets_ clientMv
    tid <- liftIO $ readMVar k
    liftIO $ killThread tid

listen :: KevinIO ()
listen = fix (\f -> flip catches errHandlers $ do
                    k   <- get_
                    pkt <- io $ parse <$> readServer k
                    respond pkt (pktCommand pkt)
                    f)

-- main responder
respond :: Packet -> T.Text -> KevinIO ()
respond _ "dAmnServer" = do
    s <- use_ settings
    sendLogin (s^.name) (s^.authtoken)

respond pkt "login" =
    if okay pkt
        then do
            j <- kevin $ do
                loggedIn .= True
                use joining
            mapM_ sendJoin j
        else I.sendNotice $ "Login failed: " <> pktArgs pkt ^. ix "e"

respond pkt "join" = do
    roomname <- deformatRoom $ pktParameter pkt ^. _Just
    if okay pkt
       then do
           kevin $ joining %= (roomname:)
           uname <- use_ name
           I.sendJoin uname roomname
       else I.sendNotice $ T.concat ["Couldn't join ", roomname, ": ", pktArgs pkt ^. ix "e"]

respond pkt "part" = do
    roomname <- deformatRoom $ pktParameter pkt ^. _Just
    if okay pkt
        then do
            uname <- use_ name
            removeRoom roomname
            I.sendPart uname roomname Nothing
        else I.sendNotice $ T.concat ["Couldn't part ", roomname, ": ", pktArgs pkt ^. ix "e"]

respond pkt "property" = do
    roomname <- deformatRoom $ pktParameter pkt ^. _Just
    case pktArgs pkt ^. ix "p" of
        "privclasses" -> setPrivclasses roomname . parsePrivclasses $ pktBody pkt ^. _Just . to decodeUtf8

        "topic" -> do
            uname <- use_ name
            let setter = fromMaybe uname $ pktArgs pkt ^. at "by"
                topic = T.replace "\n" " - " . entityDecode . tablumpDecode $ pktBody pkt ^. _Just . to decodeUtf8
                time = pktArgs pkt ^. ix "ts"
            I.sendTopic uname roomname setter topic time

        "title" -> setTitle roomname (T.replace "\n" " - " . entityDecode . tablumpDecode $ pktBody pkt ^. _Just . to decodeUtf8)

        "members" -> do
            k <- get_
            let members = map (mkUser roomname (k^.privclasses) . parse)
                        . init . T.splitOn "\n\n"
                        $ pktBody pkt ^. _Just . to decodeUtf8
                pc      = privclass . head . filter (\x -> username x == k^.name) $ members
                n       = nub members
            setUsers roomname members
            when (roomname `elem` k^.joining) $ do
                I.sendUserList (k^.name) n roomname
                pclevel <- getPrivclassLevel roomname pc
                I.sendSetUserMode (k^.name) roomname pclevel
                kevin $ joining %= delete roomname

        "info" -> do
            us <- use_ name
            curtime <- io $ floor <$> getPOSIXTime
            let fixedPacket = parse . T.init
                            . T.replace "\n\nusericon" "\nusericon"
                            . decodeUtf8 . render $ pkt
                uname = T.drop 6 $ pktParameter pkt ^. _Just
                rn    = pktArgs fixedPacket ^. ix "realname"
                conns :: [(Int, Int, [T.Text])]
                conns = map (\pk ->
                            let x = parse $ "conn" <> pk
                             in ( read (T.unpack $ pktArgs x ^. ix "online") :: Int
                                , read (T.unpack $ pktArgs x ^. ix "idle"  ) :: Int
                                , map (T.drop 8) . filter (not . T.null) . T.splitOn "\n\n" $ pktBody x ^. _Just . to decodeUtf8
                                )) . tail . T.splitOn "conn" $ pktBody fixedPacket ^. _Just . to decodeUtf8
                allRooms            = nub $ conns >>= (\(_,_,c) -> c)
                (onlinespan,idle,_) = minimumBy (comparing (view _1)) conns
                signon              = curtime - onlinespan
            I.sendWhoisReply us uname (entityDecode rn) allRooms idle signon

        q -> klogError $ "Unrecognized property " ++ T.unpack q

respond spk "recv" = deformatRoom (pktParameter spk ^. _Just) >>= \roomname ->
    case pktCommand pkt of
        "join" -> do
            let usname = pktParameter pkt ^. _Just
            pcs <- gets_ $ view privclasses
            countUser <- numUsers roomname usname
            let us = mkUser roomname pcs modifiedPkt
            addUser roomname us
            if countUser == 0
               then do
                   I.sendJoin usname roomname
                   pclevel <- getPrivclassLevel roomname $ pktArgs modifiedPkt ^. ix "pc"
                   I.sendSetUserMode usname roomname pclevel
               else I.sendNoticeClone (username us) (succ countUser) roomname

        "part" -> do
            let uname = pktParameter pkt ^. _Just
            removeUser roomname uname
            countUser <- numUsers roomname uname
            if countUser < 1
               then I.sendPart uname roomname $ pktArgs pkt ^. at "r"
               else I.sendNoticeUnclone uname countUser roomname

        "msg" -> do
            let uname = arg "from"
                msg   = pktBody pkt ^. _Just . to decodeUtf8
            un_ <- use_ name
            unless (un_ == uname) $ I.sendChanMsg uname roomname (entityDecode $ tablumpDecode msg)

        "action" -> do
            let uname = arg "from"
                msg   = pktBody pkt ^. _Just . to decodeUtf8
            un_ <- use_ name
            unless (un_ == uname) $ I.sendChanAction uname roomname (entityDecode $ tablumpDecode msg)

        "privchg" -> do
            let user  = pktParameter pkt ^. _Just
                by    = arg "by"
                newPc = arg "pc"
            oldPc      <- getPrivclass roomname user
            oldPcLevel <- getPrivclassLevel roomname (fromMaybe "" oldPc)
            newPcLevel <- getPrivclassLevel roomname newPc
            setUserPrivclass roomname user newPc
            I.sendRoomNotice roomname $ T.concat [ user, " has been moved"
                                                 , maybe "" (" from " <>) oldPc
                                                 , " to ", newPc, " by ", by
                                                 ]
            I.sendChangeUserMode user roomname oldPcLevel newPcLevel

        "kicked" -> do
            let uname = pktParameter pkt ^. _Just
            removeUserAll roomname uname
            I.sendKick uname (arg "by") roomname $ pktBody pkt ^. traverse . to decodeUtf8 ^? notNull_

        "admin" -> case pktParameter pkt ^. _Just of
                       "create"    -> I.sendRoomNotice roomname $
                           T.concat [ "Privclass ", arg "name", " created by ", arg "by"
                                    , " with: ", arg "privs" ]
                       "update"    -> I.sendRoomNotice roomname $
                           T.concat [ "Privclass ", arg "name", " updated by ", arg "by"
                                    , " with: ", arg "privs" ]
                       "rename"    -> I.sendRoomNotice roomname $
                           T.concat [ "Privclass ", arg "prev", " renamed to ", arg "name"
                                    , " by ", arg "by" ]
                       "move"      -> I.sendRoomNotice roomname $
                           T.concat [ arg "n", " users in privclass ", arg "prev", " moved to "
                                    , arg "name", " by ", arg "by" ]
                       "remove"    -> I.sendRoomNotice roomname $
                           T.concat [ "Privclass", arg "name", " removed by ", arg "by" ]
                       "show"      -> mapM_ (I.sendRoomNotice roomname)
                                    . T.splitOn "\n" $ pktBody pkt ^. _Just . to decodeUtf8
                       "privclass" -> I.sendRoomNotice roomname $ "Admin error: " <> arg "e"
                       q           -> klogError $ "Unknown admin packet type " ++ show q

        x -> klogError $ "Unknown packet type " ++ show x

    where pkt         = pktSubpacket' spk ^?! _Just
          modifiedPkt = pkt
          arg s       = pktArgs pkt ^. ix s

respond pkt "kicked" = do
    roomname <- deformatRoom $ pktParameter pkt ^. _Just
    uname <- use_ name
    removeRoom roomname
    I.sendKick uname (pktArgs pkt ^. ix "by") roomname $ pktBody pkt ^. traverse . to decodeUtf8 ^? notNull_

respond pkt "send" = I.sendNotice $ "Send error: " <> pkt ^. pktArgsL . ix "e"

respond _ "ping" = get_ >>= \k -> io . writeServer k $ ("pong\n\0" :: T.Text)

respond _ str = klog Yellow $ "Got the packet called " ++ T.unpack str


mkUser :: Chatroom -> PrivclassStore -> Packet -> User
mkUser room st p = User {
              username       = p ^?! pktParameterL . _Just
            , privclass      = g "pc"
            , privclassLevel = fromMaybe 0 $ st ^. at room >>= view (at $ g "pc")
            , symbol         = g "symbol"
            , realname       = entityDecode $ g "realname"
            , typename       = g "typename"
            , gpc            = g "gpc"
            }
    where g s = p ^. pktArgsL . ix s

errHandlers :: [Handler KevinIO ()]
errHandlers = [ handler _KevinException $ klogError . show
              , handler _IOException (\e -> klogError ("server: " ++ show e) >> E.throw e) ]
