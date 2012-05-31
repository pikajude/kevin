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
    
import qualified Data.Text as T
import qualified Data.Map as M
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.State
import Control.Monad.STM (atomically)
import Kevin.Settings

data Kevin = Kevin { damn :: Handle
                   , irc :: Handle
                   , dChan :: Chan T.Text
                   , iChan :: Chan T.Text
                   , settings :: Settings
                   , users :: UserStore
                   , privclasses :: PrivclassStore
                   , titles :: TitleStore
                   , toJoin :: [T.Text]
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

type Chatroom = T.Text

data User = User { username :: T.Text
                 , privclass :: T.Text
                 , privclassLevel :: Int
                 , symbol :: T.Text
                 , realname :: T.Text
                 , typename :: T.Text
                 , gpc :: T.Text
                 } deriving (Eq, Show)

type UserStore = M.Map Chatroom [User]

type Privclasses = M.Map T.Text Int
type PrivclassStore = M.Map Chatroom Privclasses
type Privclass = (T.Text, Int)

type Title = T.Text
type TitleStore = M.Map Chatroom Title