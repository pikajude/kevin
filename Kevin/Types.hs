module Kevin.Types (
    Kevin(..),
    KevinIO,
    Privclass,
    Chatroom,
    User(..),
    Title,
    PrivclassStore,
    UserStore,
    TitleStore,
    getK,
    getsK,
    putK,
    modifyK
) where
    
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.State
import Control.Monad.STM (atomically)
import Kevin.Settings

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   , settings :: Settings
                   , users :: UserStore
                   , privclasses :: PrivclassStore
                   , titles :: TitleStore
                   , toJoin :: [B.ByteString]
                   , loggedIn :: Bool
                   , logger :: Chan String
                   }

type KevinIO = StateT (TVar Kevin) IO

getK :: KevinIO Kevin
getK = get >>= liftIO . readTVarIO

putK :: Kevin -> KevinIO ()
putK k = get >>= io . atomically . flip writeTVar k

getsK :: (Kevin -> a) -> KevinIO a
getsK = flip liftM getK

modifyK :: (Kevin -> Kevin) -> KevinIO ()
modifyK f = do
    var <- get
    (io . atomically) $ modifyTVar var f

io :: MonadIO m => IO a -> m a
io = liftIO

type Chatroom = B.ByteString

data User = User { username :: B.ByteString
                 , privclass :: B.ByteString
                 , privclassLevel :: Int
                 , symbol :: B.ByteString
                 , realname :: B.ByteString
                 , typename :: B.ByteString
                 , gpc :: B.ByteString
                 } deriving (Eq)
type UserStore = M.Map Chatroom [User]

type Privclasses = M.Map B.ByteString Int
type PrivclassStore = M.Map Chatroom Privclasses
type Privclass = (B.ByteString, Int)

type Title = B.ByteString
type TitleStore = M.Map Chatroom Title