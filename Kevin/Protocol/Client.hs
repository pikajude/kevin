module Kevin.Protocol.Client (
    cleanup,
    listen,
    errHandlers,
    getAuthInfo
) where

import qualified Data.ByteString.Char8 as B
import Kevin.Base
import Kevin.Util.Logger
import Kevin.Settings
import Control.Monad.State

cleanup :: KevinIO ()
cleanup = io $ klog Green "cleanup client"

listen :: KevinIO ()
listen = flip catches errHandlers $ do
    k <- ask
    line <- io $ readClient k
    unless (B.null line) $ do
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

getAuthInfo :: Handle -> StateT Settings IO ()
getAuthInfo handle = do
    liftIO $ B.hPut handle "Please enter your username: "
    user <- liftIO $ B.hGetLine handle
    modify (setUsername $ B.init user)