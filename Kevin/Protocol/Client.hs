module Kevin.Protocol.Client (
    initialize,
    cleanup,
    errHandlers
) where

import Kevin.Base
import Kevin.Util.Logger
import System.IO
import Control.Exception

initialize :: Kevin -> IO ()
initialize = const $ klog Green "initialize client"

cleanup :: Kevin -> IO ()
cleanup = const $ klog Green "cleanup client"

errHandlers :: [Handler ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
    LostServer -> klogError "Lost server connection, DCing client"
    x -> throwIO x)]