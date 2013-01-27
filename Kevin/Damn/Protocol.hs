module Kevin.Damn.Protocol (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Control.Applicative ((<$>))
import Control.Exception.Lens
import Data.List (delete, nub, minimumBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Kevin.Base
import Kevin.Damn.Packet
import Kevin.Damn.Protocol.Send
import qualified Kevin.IRC.Protocol.Send as I
import Kevin.Util.Entity
import Kevin.Util.Logger
import Kevin.Util.Tablump

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = klog Blue "cleanup server"

listen :: KevinIO ()
listen = fix (\f -> flip catches errHandlers $ do
    k <- get_
    pkt <- io $ parsePacket <$> readServer k
    respond pkt (view command pkt)
    f)

-- main responder
respond :: Packet -> T.Text -> KevinIO ()
respond _ "dAmnServer" = do
    s <- use_ settings
    sendLogin (s^.name) (s^.authtoken)

respond pkt "login" = if okay pkt
    then do
        j <- kevin $ do
           loggedIn .= True
           use joining
        mapM_ sendJoin j
    else I.sendNotice $ "Login failed: " <> pkt ^. args.ix "e"

respond pkt "join" = do
    roomname <- deformatRoom $ pkt^.parameter._Just
    if okay pkt
        then do
            kevin $ joining %= (roomname:)
            uname <- use_ name
            I.sendJoin uname roomname
        else I.sendNotice $ T.concat
            ["Couldn't join ", roomname, ": ", pkt^.args.ix "e"]

respond pkt "part" = do
    roomname <- deformatRoom $ pkt^.parameter._Just
    if okay pkt
        then do
            uname <- use_ name
            removeRoom roomname
            I.sendPart uname roomname Nothing
        else I.sendNotice $ T.concat
            ["Couldn't part ", roomname, ": ", pkt^.args.ix "e"]

respond pkt "property" = do
    roomname <- deformatRoom (pkt^.parameter._Just)
    case pkt^.args.ix "p" of
        "privclasses" -> setPrivclasses roomname . parsePrivclasses
            $ pkt^.body._Just

        "topic" -> do
            uname <- use_ name
            I.sendTopic uname roomname
                        (fromMaybe uname $ pkt^.args.at "by")
                        (T.replace "\n" " - " . entityDecode
                            . tablumpDecode $ pkt^.body._Just)
                        (pkt^.args.ix "ts")

        "title" -> setTitle roomname (T.replace "\n" " - " . entityDecode
                                     . tablumpDecode $ pkt^.body._Just)

        "members" -> do
            k <- get_
            let members = map (mkUser roomname (k^.privclasses) . parsePacket)
                        . init . T.splitOn "\n\n"
                        $ pkt^.body._Just
                pc = privclass . head . filter (\x -> username x == k^.name)
                   $ members
                n = nub members
            setUsers roomname members
            when (roomname `elem` k^.joining) $ do
                I.sendUserList (k^.name) n roomname
                pclevel <- getPrivclassLevel roomname pc
                I.sendSetUserMode (k^.name) roomname pclevel
                kevin $ joining %= delete roomname

        "info" -> do
            us <- use_ name
            curtime <- io $ floor <$> getPOSIXTime
            let fixedPacket = parsePacket . T.init
                            . T.replace "\n\nusericon" "\nusericon"
                            . readable $ pkt
                uname = T.drop 6 $ pkt^.parameter._Just
                rn = fixedPacket^.args.ix "realname"
                conns = map (\pk ->
                    let x = parsePacket $ "conn" <> pk
                    in ( read (T.unpack $ x^.args.ix "online") :: Int
                       , read (T.unpack $ x^.args.ix "idle") :: Int
                       , map (T.drop 8) . filter (not . T.null)
                       . T.splitOn "\n\n" $ x^.body._Just
                    )
                  ) . tail . T.splitOn "conn"
                    $ fixedPacket^.body._Just
                allRooms = nub $ conns >>= (\(_,_,c) -> c)
                (onlinespan,idle,_) = minimumBy (comparing (view _1)) conns
                signon = curtime - onlinespan
            I.sendWhoisReply us uname (entityDecode rn) allRooms idle signon

        q -> klogError $ "Unrecognized property " ++ T.unpack q

respond spk "recv" = deformatRoom (spk^.parameter._Just) >>= \roomname ->
    case pkt^.command of
    "join" -> do
        let usname = pkt^.parameter._Just
        pcs <- gets_ $ view privclasses
        countUser <- numUsers roomname usname
        let us = mkUser roomname pcs modifiedPkt
        addUser roomname us
        if countUser == 0
            then do
                I.sendJoin usname roomname
                pclevel <- getPrivclassLevel roomname
                    $ modifiedPkt^.args.ix "pc"
                I.sendSetUserMode usname roomname pclevel
            else I.sendNoticeClone (username us) (succ countUser) roomname

    "part" -> do
        let uname = pkt^.parameter._Just
        removeUser roomname uname
        countUser <- numUsers roomname uname
        if countUser < 1
            then I.sendPart uname roomname $ pkt^.args.at "r"
            else I.sendNoticeUnclone uname countUser roomname

    "msg" -> do
        let uname = arg "from"
            msg   = pkt^.body._Just
        un <- use_ name
        unless (un == uname) $ I.sendChanMsg uname roomname
            (entityDecode $ tablumpDecode msg)

    "action" -> do
        let uname = arg "from"
            msg   = pkt^.body._Just
        un <- use_ name
        unless (un == uname) $ I.sendChanAction uname roomname
            (entityDecode $ tablumpDecode msg)

    "privchg" -> do
        let user = pkt^.parameter._Just
            by = arg "by"
            newPc = arg "pc"
        oldPc <- getPrivclass roomname user
        oldPcLevel <- getPrivclassLevel roomname (fromMaybe "" oldPc)
        newPcLevel <- getPrivclassLevel roomname newPc
        setUserPrivclass roomname user newPc
        I.sendRoomNotice roomname $ T.concat
            [user, " has been moved"
            , maybe "" (" from " <>) oldPc
            , " to ", newPc, " by ", by
            ]
        I.sendChangeUserMode user roomname oldPcLevel newPcLevel

    "kicked" -> do
        let uname = pkt^.parameter._Just
        removeUserAll roomname uname
        I.sendKick uname (arg "by") roomname $ pkt^.body.traverse ^? notNull_

    "admin" -> case pkt^.parameter._Just of
        "create" -> I.sendRoomNotice roomname $
            T.concat ["Privclass ", arg "name"
                     , " created by ", arg "by"
                     , " with: ", arg "privs"]
        "update" -> I.sendRoomNotice roomname $
            T.concat ["Privclass ", arg "name"
                     , " updated by ", arg "by"
                     , " with: ", arg "privs"]
        "rename" -> I.sendRoomNotice roomname $
            T.concat ["Privclass ", arg "prev"
                     , " renamed to ", arg "name"
                     , " by ", arg "by"]
        "move"   -> I.sendRoomNotice roomname $
            T.concat [ arg "n", " users in privclass "
                     , arg "prev", " moved to "
                     , arg "name", " by ", arg "by"]
        "remove" -> I.sendRoomNotice roomname $
            T.concat ["Privclass", arg "name", " removed by ", arg "by"]
        "show"   -> mapM_ (I.sendRoomNotice roomname) . T.splitOn "\n"
                        $ pkt^.body._Just
        "privclass" -> I.sendRoomNotice roomname $ "Admin error: " <> arg "e"
        q -> klogError $ "Unknown admin packet type " ++ show q

    x -> klogError $ "Unknown packet type " ++ show x

    where
        pkt = fromJust $ subPacket spk
        modifiedPkt = parsePacket (T.replace "\n\npc" "\npc" $ pkt^.body._Just)
        arg s = pkt^.args.ix s

respond pkt "kicked" = do
    roomname <- deformatRoom $ pkt^.parameter._Just
    uname <- use_ name
    removeRoom roomname
    I.sendKick uname (pkt^.args.ix "by") roomname
        $ pkt^.body.traverse ^? notNull_

respond pkt "send" = I.sendNotice $ "Send error: " <> pkt^.args.ix "e"

respond _ "ping" = get_ >>= \k -> io . writeServer k $ ("pong\n\0" :: T.Text)

respond _ str = klog Yellow $ "Got the packet called " ++ T.unpack str


mkUser :: Chatroom -> PrivclassStore -> Packet -> User
mkUser room st p = User (p^.parameter._Just)
                       (g "pc")
                       (fromMaybe 0 $ st^.at room >>= (^.at (g "pc")))
                       (g "symbol")
                       (entityDecode $ g "realname")
                       (g "typename")
                       (g "gpc")
    where
        g s = p^.args.ix s

errHandlers :: [Handler KevinIO ()]
errHandlers = [ handler_ _KevinException
                    $ klogError "Malformed communication from server"
              , handler _IOException (\e -> klogError $ "server: " ++ show e) ]
