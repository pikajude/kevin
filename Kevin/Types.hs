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
    get_,
    gets_,
    put_,
    modify_
) where
    
import qualified Data.Text as T
import qualified Data.Map as M
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
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
                   , joining :: [T.Text]
                   , loggedIn :: Bool
                   , logger :: Chan String
                   }

type KevinIO = ReaderT (TVar Kevin) IO

get_ :: KevinIO Kevin
get_ = ask >>= liftIO . readTVarIO

put_ :: Kevin -> KevinIO ()
put_ k = ask >>= io . atomically . flip writeTVar k

gets_ :: (Kevin -> a) -> KevinIO a
gets_ = flip liftM get_

modify_ :: (Kevin -> Kevin) -> KevinIO ()
modify_ f = do
    var <- ask
    io . atomically $ modifyTVar var f

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