{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kevin.Types (
    Kevin(Kevin, damn, irc, dChan, iChan, logger),
    KevinIO,
    KevinS,
    Privclass,
    Chatroom,
    User(..),
    Title,
    PrivclassStore,
    UserStore,
    TitleStore,
    kevin,
    use_,
    get_,
    gets_,

    -- lenses
    users, privclasses, titles, joining, loggedIn,

    -- other accessors
    settings,

    if'
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.Reader
import Control.Monad.STM (STM, atomically)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Kevin.Settings
import System.IO

if' :: Bool -> a -> a -> a
if' x y z = if x then y else z

type Chatroom = T.Text

data User = User { username       :: T.Text
                 , privclass      :: T.Text
                 , privclassLevel :: Int
                 , symbol         :: T.Text
                 , realname       :: T.Text
                 , typename       :: T.Text
                 , gpc            :: T.Text
                 } deriving (Eq, Show)

type UserStore      = M.Map Chatroom [User]

type Privclasses    = M.Map T.Text Int
type PrivclassStore = M.Map Chatroom Privclasses
type Privclass      = (T.Text, Int)

type Title          = T.Text
type TitleStore     = M.Map Chatroom Title

data Kevin = Kevin { damn           :: Handle
                   , irc            :: Handle
                   , dChan          :: Chan T.Text
                   , iChan          :: Chan T.Text
                   , _kevinSettings :: Settings
                   , _users         :: UserStore
                   , _privclasses   :: PrivclassStore
                   , _titles        :: TitleStore
                   , _joining       :: [T.Text]
                   , _loggedIn      :: Bool
                   , logger         :: Chan String
                   } deriving Typeable

makeLenses ''Kevin

instance HasSettings Kevin where
  settings = kevinSettings

type KevinS = StateT Kevin STM

kevin :: KevinS a -> KevinIO a
kevin m = ask >>= \v -> liftIO $ atomically $ do s <- readTVar v
                                                 (a, t) <- runStateT m s
                                                 writeTVar v t
                                                 return a

type KevinIO = ReaderT (TVar Kevin) IO

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
deriving instance Typeable ReaderT
#else
instance Typeable1 (ReaderT (TVar Kevin) IO) where
    typeOf1 _ = mkTyConApp (mkTyCon3 "kevin" "Kevin.Types" "KevinIO") []
#endif

use_ :: Getting a Kevin a -> KevinIO a
use_ = gets_ . view

get_ :: KevinIO Kevin
get_ = ask >>= liftIO . readTVarIO

gets_ :: (Kevin -> a) -> KevinIO a
gets_ = flip liftM get_
