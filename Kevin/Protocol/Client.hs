module Kevin.Protocol.Client (
    cleanup,
    errHandlers,
    getAuthInfo
) where

import Kevin.Base
import Kevin.Util.Logger

cleanup :: Kevin -> IO ()
cleanup = const $ klog Green "cleanup client"

errHandlers :: [Handler ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
    LostServer -> klogError "Lost server connection, DCing client"
    x -> throwIO x)]

getAuthInfo :: Handle -> IO (String, String)
getAuthInfo k = do
    return ("hi", "bye")