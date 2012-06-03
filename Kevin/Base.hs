module Kevin.Base (
    Kevin(..),
    KevinIO,
    KevinException(..),
    KServerPacket(..),
    KClientPacket(..),
    KevinServer(..),
    User(..),
    Privclass,
    Chatroom,
    Title,
    UserStore,
    PrivclassStore,
    TitleStore,
    
    -- * Modifiers
    addUser,
    removeUser,
    setUsers,
    onUsers,
    numUsers,
    
    addPrivclass,
    setPrivclasses,
    onPrivclasses,
    getPcLevel,
    
    logIn,
    
    addToJoin,
    
    setTitle,
    onTitles,
    
    -- * Exports
    module K,
    
    -- * Working with KevinState
    io,
    runPrinter,
    getK,
    putK,
    getsK,
    modifyK
) where

import Kevin.Util.Logger
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as T (hGetLine, hPutStr)
import qualified Data.Text.Encoding as T
import Data.List (intercalate, nub, findIndices)
import System.IO as K (Handle, hClose, hIsClosed, hGetChar)
import Control.Exception as K (IOException)
import Network as K
import Control.Monad.Reader
import Control.Monad.State as K
import Control.Concurrent as K (forkIO)
import Control.Concurrent.Chan as K
import Control.Concurrent.STM.TVar as K
import Control.Monad.CatchIO as K
import Kevin.Settings as K
import qualified Data.Map as M
import Data.Typeable
import Kevin.Types

runPrinter :: Chan T.Text -> Handle -> IO ()
runPrinter ch h = void $ forkIO $ forever $ readChan ch >>= T.hPutStr h . T.encodeUtf8

io :: MonadIO m => IO a -> m a
io = liftIO

class KServerPacket a where
    asStringS :: a -> T.Text

class KClientPacket a where
    asStringC :: a -> T.Text

class KevinServer a where
    readClient, readServer :: a -> IO T.Text
    writeServer :: (KServerPacket p) => a -> p -> IO ()
    writeClient :: (KClientPacket p) => a -> p -> IO ()
    closeClient, closeServer :: a -> IO ()

data KevinException = ParseFailure
    deriving (Show, Typeable)

instance Exception KevinException

-- actions
                   
padLines :: Int -> T.Text -> String
padLines len b = let (first:rest) = lines $ T.unpack b in (++) (first ++ "\n") $ intercalate "\n" $ map (replicate len ' ' ++) rest

hGetSep :: Char -> Handle -> IO String
hGetSep sep h = do
    ch <- hGetChar h
    if ch == sep
        then return ""
        else fmap (ch:) $ hGetSep sep h

instance KevinServer Kevin where
    readClient k = do
        line <- fmap T.decodeUtf8 $ T.hGetLine $ irc k
        klog_ (logger k) Yellow $ "client <- " ++ padLines 10 line
        return $ T.init line
    readServer k = do
        line <- fmap T.pack $ hGetSep '\x0' $ damn k
        klog_ (logger k) Cyan $ "server <- " ++ padLines 10 line
        return line
    
    writeClient k str = do
        let pkt = asStringC str
        klog_ (logger k) Blue $ "client -> " ++ padLines 10 pkt
        writeChan (iChan k) pkt
    writeServer k str = do
        let pkt = asStringS str
        klog_ (logger k) Magenta $ "server -> " ++ padLines 10 pkt
        writeChan (dChan k) pkt
    
    closeClient = hClose . irc
    closeServer = hClose . damn

instance KServerPacket T.Text where
    asStringS = id

instance KClientPacket T.Text where
    asStringC = id

-- Kevin modifiers
logIn :: Kevin -> Kevin
logIn k = k { loggedIn = True }

addToJoin :: [T.Text] -> Kevin -> Kevin
addToJoin rooms k = k { toJoin = nub $ rooms ++ toJoin k }

addUser :: Chatroom -> User -> UserStore -> UserStore
addUser = (. return) . M.insertWith (++)

numUsers :: Chatroom -> T.Text -> UserStore -> Int
numUsers room us st = case M.lookup room st of
    Just usrs -> length $ findIndices (\u -> us == username u) usrs
    Nothing -> 0

removeUser :: Chatroom -> T.Text -> UserStore -> UserStore
removeUser room us = M.adjust (removeOne' (\x -> username x == us)) room

removeOne' :: (User -> Bool) -> [User] -> [User]
removeOne' _ [] = []
removeOne' f (x:xs) = if f x then xs else x:removeOne' f xs

setUsers :: Chatroom -> [User] -> UserStore -> UserStore
setUsers = M.insert

onUsers :: (UserStore -> UserStore) -> Kevin -> Kevin
onUsers f k = k { users = f (users k) }

addPrivclass :: Chatroom -> Privclass -> PrivclassStore -> PrivclassStore
addPrivclass room (p,i) = M.insertWith M.union room (M.singleton p i)

setPrivclasses :: Chatroom -> [Privclass] -> PrivclassStore -> PrivclassStore
setPrivclasses room ps = M.insert room (M.fromList ps)

onPrivclasses :: (PrivclassStore -> PrivclassStore) -> Kevin -> Kevin
onPrivclasses f k = k { privclasses = f (privclasses k) }

getPcLevel :: Chatroom -> T.Text -> PrivclassStore -> Maybe Int
getPcLevel room pcname store = M.lookup room store >>= M.lookup pcname

setTitle :: Chatroom -> Title -> TitleStore -> TitleStore
setTitle = M.insert

onTitles :: (TitleStore -> TitleStore) -> Kevin -> Kevin
onTitles f k = k { titles = f (titles k) }