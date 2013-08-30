module Kevin.Chatrooms (
    removeRoom,
    
    addUser,
    setUsers,
    removeUser,
    removeUserAll,
    numUsers,
    
    setPrivclasses,
    getPrivclass,
    getPrivclassLevel,
    setUserPrivclass,
    
    setTitle
) where

import Control.Applicative
import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Kevin.Types

deleteBy' :: (a -> Bool) -> [a] -> [a]
deleteBy' f (x:xs) = if f x then xs else x:deleteBy' f xs
deleteBy' _ []     = []

removeRoom :: Chatroom -> KevinIO ()
removeRoom c = kevin $ privclasses.at c .= Nothing >> users.at c .= Nothing

addUser :: Chatroom -> User -> KevinIO ()
addUser ch us = kevin $ users.ix ch %= (us:)

numUsers :: Chatroom -> T.Text -> KevinIO Int
numUsers ch us = do st <- gets_ $ view users
                    case st^.at ch of Just usrs -> return . length $ findIndices (\u -> us == username u) usrs
                                      Nothing   -> return 0

removeUser :: Chatroom -> T.Text -> KevinIO ()
removeUser ch us = kevin $ users.ix ch %= deleteBy' ((== us) . username)

removeUserAll :: Chatroom -> T.Text -> KevinIO ()
removeUserAll ch us = kevin $ users.ix ch %= filter ((/= us) . username)

setUsers :: Chatroom -> [User] -> KevinIO ()
setUsers ch uss = kevin $ users.at ch ?= uss

setPrivclasses :: Chatroom -> [Privclass] -> KevinIO ()
setPrivclasses room ps = kevin $ privclasses.at room ?= M.fromList ps

getPrivclass :: Chatroom -> T.Text -> KevinIO (Maybe T.Text)
getPrivclass room user = do st <- gets_ $ view users
                            case st^.at room of Just qs -> return $ privclass <$> listToMaybe (filter ((== user) . username) qs)
                                                Nothing -> return Nothing
        
getPrivclassLevel :: Chatroom -> T.Text -> KevinIO Int
getPrivclassLevel room pc = do st <- gets_ $ view privclasses
                               return . fromMaybe 0 $ st^.at room >>= (^.at pc)

setUserPrivclass :: Chatroom -> T.Text -> T.Text -> KevinIO ()
setUserPrivclass room user pc = do pclevel <- getPrivclassLevel room pc
                                   kevin $ users.ix room.traverse.filtered ((user ==) . username) %= (\u -> u {privclass = pc, privclassLevel = pclevel})

setTitle :: Chatroom -> T.Text -> KevinIO ()
setTitle ch t = kevin $ titles.at ch ?= t
