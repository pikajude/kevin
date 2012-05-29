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
import Kevin.Damn.Protocol.Send
import qualified Kevin.IRC.Protocol.Send as I

initialize :: KevinIO ()
initialize = sendHandshake

cleanup :: KevinIO ()
cleanup = io $ klog Blue "cleanup server"

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
    let uname = username set
        token = authtoken set
    sendLogin uname token

respond pkt "login" = do
    uname <- getsK (username . settings)
    if okay pkt
        then do
            io $ klog Green $ "Logged in as " ++ B.unpack uname
            modifyK logIn
            getsK toJoin >>= mapM_ sendJoin
        else io $ klogError $ "Login failed: " ++ B.unpack (getArg "e" pkt)

respond pkt "join" = if okay pkt
    then do
        uname <- getsK (username . settings)
        (I.sendJoin uname . deformatRoom . fromJust) $ parameter pkt
    else io $ klogError $ "Join failed: " ++ B.unpack (getArg "e" pkt)

respond pkt "property" = case getArg "p" pkt of
    "privclasses" -> do
        let pcs = parsePrivclasses $ fromJust $ body pkt
        modifyK (onPrivclasses (foldr (.) id (map (setPrivclass roomname) pcs)))
    "topic" -> do
        uname <- getsK (username . settings)
        I.sendTopic uname roomname (getArg "by" pkt) (fromJust (body pkt)) (getArg "ts" pkt)
    "title" -> modifyK (onTitles (setTitle roomname (fromJust (body pkt))))
    "members" -> do
        let members = map parsePacket $ init $ splitOn "\n\n" $ fromJust (body pkt)
        io $ print members
    where
        roomname = (deformatRoom . fromJust . parameter) pkt

respond _ str = io $ print $ "Got the packet called " `B.append` str


errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
                   LostClient -> io $ klogError "Lost client connection, DCing server"
                   ParseFailure -> io $ klogError "Malformed communication from server"
                   _ -> io $ klogError "Got the wrong exception"),
               Handler (\(e :: IOException) -> io $ klogError $ "server: " ++ show e)]