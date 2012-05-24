module Kevin.Protocol.Server (
    initialize,
    cleanup,
    listen,
    errHandlers
) where

import Prelude hiding (null)
import Kevin.Base
import Kevin.Util.Logger

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
errHandlers = [Handler (\(_ :: KevinException) -> io $ klogError "Lost client connection, DCing server"),
               Handler (\(e :: IOException) -> do
                   clId <- asks clientId
                   io $ do
                       klogError $ "server: " ++ show e
                       tid <- takeMVar clId
                       throwTo tid LostServer)]