module Kevin.Protocol.Client (
    cleanup,
    listen,
    errHandlers,
    getAuthInfo
) where

import Prelude hiding (null)
import Kevin.Base
import Kevin.Util.Logger

cleanup :: KevinIO ()
cleanup = io $ klog Green "cleanup client"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- ask
    line <- io $ readClient k
    unless (null line) $ do
        io $ print line
        listen

errHandlers :: [Handler KevinIO ()]
errHandlers = [Handler (\(_ :: KevinException) -> io $ klogError "Lost server connection, DCing client"),
               Handler (\(e :: IOException) -> do
                   servId <- asks serverId
                   io $ do
                       klogError $ "client: " ++ show e
                       tid <- takeMVar servId
                       throwTo tid LostClient)]

getAuthInfo :: Handle -> IO (String, String)
getAuthInfo _ = return ("hi", "bye")