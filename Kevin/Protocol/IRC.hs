module Kevin.Protocol.IRC (
    cleanup,
    listen,
    errHandlers,
    getAuthInfo
) where

import qualified Data.ByteString.Char8 as B
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Util.Token
import Kevin.Packet.IRC
import qualified Kevin.Protocol.Damn.Send as D

type KevinState = StateT Settings IO

cleanup :: KevinIO ()
cleanup = io $ klog Green "cleanup client"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- getK
    pkt <- io $ fmap parsePacket $ readClient k
    respond pkt (command pkt)
    listen

respond :: Packet -> B.ByteString -> KevinIO ()
respond pkt "JOIN" = do
    l <- getsK loggedIn
    if l
        then mapM_ D.sendJoin rooms
        else do
            modifyK (addToJoin rooms)
            getsK toJoin >>= \x -> io $ klog Magenta $ show x
    where
        rooms = B.split ',' $ head $ params pkt
respond _ str = io $ klogError $ B.unpack str

errHandlers :: [Handler KevinIO ()]
errHandlers = [
    Handler (\(e :: KevinException) -> case e of
        LostServer -> io $ klogError "Lost server connection, DCing client"
        ParseFailure -> io $ klogError "Bad communication from client"
        _ -> io $ klogError "Got the wrong exception"),
        
    Handler (\(e :: IOException) -> io $ klogError $ "client: " ++ show e)]

-- * Authentication-getting function
notice :: Handle -> B.ByteString -> IO ()
notice h str = klog Blue ("client -> " ++ B.unpack asStr) >> B.hPut h asStr
    where
        asStr = asStringC $ Packet Nothing "NOTICE" ["AUTH", str]

getAuthInfo :: Handle -> Bool -> KevinState ()
getAuthInfo handle authRetry = do
    pkt <- io $ fmap parsePacket $ B.hGetLine handle
    io $ klog Yellow $ "client <- " ++ B.unpack (asStringC pkt)
    case command pkt of
        "PASS" -> do
            modify (setPassword $ head $ params pkt)
            if authRetry
               then checkToken handle
               else getAuthInfo handle False
        "NICK" -> do
            modify (setUsername $ head $ params pkt)
            getAuthInfo handle False
        "USER" -> welcome handle
        _ -> do
            io $ klogError $ "invalid packet: " ++ show pkt
            when authRetry $ getAuthInfo handle True

welcome :: Handle -> KevinState ()
welcome handle = do
    nick <- gets username
    mapM_ (\x -> io $ klog Blue ("client -> " ++ B.unpack (asStringC x)) >> B.hPut handle (asStringC x)) [
        Packet { prefix = hostname
               , command = "001"
               , params = [nick, B.concat ["Welcome to dAmnServer ", nick, "!", nick, "@chat.deviantart.com"]]
               },
        Packet { prefix = hostname
               , command = "002"
               , params = [nick, "Your host is chat.deviantart.com, running dAmnServer 0.3"]
               },
        Packet { prefix = hostname
               , command = "003"
               , params = [nick, "This server was created Thu Apr 28 1994 at 05:30:00 EDT"]
               },
        Packet { prefix = hostname
               , command = "004"
               , params = [nick, "chat.deviantart.com", "dAmnServer0.3", "qaohv", "i"]
               },
        Packet { prefix = hostname
               , command = "375"
               , params = [nick, "- chat.deviantart.com Message of the day -"]
               },
        Packet { prefix = hostname
               , command = "372"
               , params = [nick, "- deviantART chat on IRC brought to you by kevin" `B.append` VERSION `B.append` ", created"]
               },
        Packet { prefix = hostname
               , command = "375"
               , params = [nick, "- and maintained by Joel Taylor <http://otter.github.com>"]
               },
        Packet { prefix = hostname
               , command = "376"
               , params = [nick, "End of MOTD command"]
               }
        ]
    checkToken handle
    where
        hostname = Just "chat.deviantart.com"

checkToken :: Handle -> KevinState ()
checkToken handle = do
    nick <- gets username
    pass <- gets password
    io $ notice handle "Fetching token..."
    tok <- io $ getToken nick pass
    case tok of
        Just t -> do
            modify (setAuthtoken t)
            io $ notice handle "Successfully authenticated."
        Nothing -> do
            io $ notice handle "Bad password, try again. (/quote pass yourpassword)"
            getAuthInfo handle True

-- * Send *to* client