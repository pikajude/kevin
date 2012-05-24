module Kevin.Protocol.Server (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Prelude hiding (catch, null)
import Kevin.Base
import Kevin.Util.Logger
import qualified Data.ByteString.Char8 as B
import Control.Monad.CatchIO

initialize :: KevinIO ()
initialize = ask >>= \k -> io $ writeServer k "dAmnClient 0.3\nagent=kevin 0.1\n\0"

cleanup :: KevinIO ()
cleanup = io $ klog Blue "cleanup server"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- ask
    line <- io $ readServer k
    unless (null line) $ do
        io $ print line
        listen

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(e :: KevinException) -> io $ klogError "Lost client connection, DCing server"),
               Handler (\(e :: IOException) -> do
                   clId <- asks clientId
                   io $ do
                       klogError $ "server: " ++ show e
                       tid <- takeMVar clId
                       throwTo tid LostServer)]