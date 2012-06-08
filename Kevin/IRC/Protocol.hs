module Kevin.IRC.Protocol (
    cleanup,
    listen,
    errHandlers,
    getAuthInfo
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Util.Token
import Kevin.Util.Entity
import Kevin.IRC.Packet
import qualified Kevin.Damn.Protocol.Send as D
import Kevin.IRC.Protocol.Send
import Control.Applicative ((<$>))
import Control.Arrow
import Data.Maybe
import qualified Data.Map as M

type KevinState = StateT Settings IO

cleanup ∷ KevinIO ()
cleanup = klog Green "cleanup client"

listen ∷ KevinIO ()
listen = fix (\f → flip catches errHandlers $ do
    k ← getK
    pkt ← io $ parsePacket <$> readClient k
    respond pkt (command pkt)
    f)

respond ∷ Packet → T.Text → KevinIO ()
respond pkt "JOIN" = do
    l ← getsK loggedIn
    if l
        then mapM_ D.sendJoin rooms
        else modifyK (addToJoin rooms)
    where
        rooms = T.splitOn "," $ head $ params pkt

respond pkt "PART" = mapM_ D.sendPart $ T.splitOn "," $ head $ params pkt

respond pkt "PRIVMSG" = do
    let (room:msg:_) = params pkt
    if "\1ACTION" `T.isPrefixOf` msg
        then do
            let newMsg = T.drop 8 $ T.init msg
            D.sendAction room $ entityEncode newMsg
        else D.sendMsg room $ entityEncode msg

respond pkt "MODE" = if length (params pkt) > 1
    then do
        let (toggle,mode) = first (=="+") $ T.splitAt 1 (params pkt !! 1)
        case mode of
            "b" → if' toggle D.sendBan D.sendUnban (head $ params pkt) (fromMaybe "*" $ unmask $ last $ params pkt)
            "o" → if' toggle D.sendPromote D.sendDemote (head $ params pkt) (last $ params pkt) Nothing
            _ → sendNotice $ "Unsupported mode " `T.append` mode
    else do
        uname ← getsK (getUsername . settings)
        sendChanMode uname (head $ params pkt)

respond pkt "PING" = sendPong (head $ params pkt)

respond pkt "WHOIS" = D.sendWhois $ head $ params pkt

respond pkt "NAMES" = do
    let (room:_) = params pkt
    (me,uss) ← getsK (getUsername . settings &&& M.lookup room . users)
    sendUserList me (fromMaybe [] uss) room

respond pkt "KICK" = let p = params pkt in D.sendKick (head p) (p !! 1) (if length p > 2 then Just $ last p else Nothing)

respond _ "QUIT" = klogError "client quit" >> undefined

respond _ str = klogError $ T.unpack str


unmask ∷ T.Text → Maybe T.Text
unmask y = case T.split (`elem` "@!") y of
    [s] → Just s
    xs → listToMaybe $ filter (not . T.isInfixOf "*") xs

errHandlers ∷ [Handler KevinIO ()]
errHandlers = [
    Handler (\(_ ∷ KevinException) → klogError "Bad communication from client"),
    Handler (\(e ∷ IOException) → klogError $ "client: " ++ show e)]

-- * Authentication-getting function
notice ∷ Handle → T.Text → IO ()
notice h str = klogNow Blue ("client → " ++ T.unpack asStr) >> T.hPutStr h asStr
    where
        asStr = asStringC $ Packet Nothing "NOTICE" ["AUTH", str]

getAuthInfo ∷ Handle → Bool → KevinState ()
getAuthInfo handle = fix (\f authRetry → do
    pkt ← io $ parsePacket <$> T.hGetLine handle
    io $ klogNow Yellow $ "client ← " ++ T.unpack (asStringC pkt)
    case command pkt of
        "PASS" → do
            modify (setPassword $ head $ params pkt)
            if authRetry
               then checkToken handle
               else f False
        "NICK" → do
            modify (setUsername $ head $ params pkt)
            f False
        "USER" → welcome handle
        _ → do
            io $ klogNow Red $ "invalid packet: " ++ show pkt
            when authRetry $ f True)

welcome ∷ Handle → KevinState ()
welcome handle = do
    nick ← gets getUsername
    mapM_ (\x → io $ klogNow Blue ("client → " ++ T.unpack (asStringC x)) >> T.hPutStr handle (asStringC x)) [
        Packet { prefix = hostname
               , command = "001"
               , params = [nick, T.concat ["Welcome to dAmnServer ", nick, "!", nick, "@chat.deviantart.com"]]
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
               , params = [nick, "chat.deviantart.com", "dAmnServer0.3", "qov", "i"]
               },
        Packet { prefix = hostname
               , command = "005"
               , params = [nick, "PREFIX=(qov)~@+"]
               },
        Packet { prefix = hostname
               , command = "375"
               , params = [nick, "- chat.deviantart.com Message of the day -"]
               },
        Packet { prefix = hostname
               , command = "372"
               , params = [nick, "- deviantART chat on IRC brought to you by kevin" `T.append` VERSION `T.append` ", created"]
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

checkToken ∷ Handle → KevinState ()
checkToken handle = do
    nick ← gets getUsername
    pass ← gets getPassword
    io $ notice handle "Fetching token..."
    tok ← io $ getToken nick pass
    case tok of
        Just t → do
            modify (setAuthtoken t)
            io $ notice handle "Successfully authenticated."
        Nothing → do
            io $ notice handle "Bad password, try again. (/quote pass yourpassword)"
            getAuthInfo handle True

-- * Send *to* client
