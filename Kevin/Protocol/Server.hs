module Kevin.Protocol.Server (
    initialize,
    cleanup,
    errHandlers
) where

import Kevin.Base
import Data.ByteString.Char8
import Kevin.Util.Logger
import System.IO
import Control.Exception

initialize :: Kevin -> IO ()
initialize kevin = writeServer kevin "dAmnClient 0.3\nagent=kevin 0.1\n\0"

cleanup :: Kevin -> IO ()
cleanup = const $ klog Blue "cleanup server"

errHandlers :: [Handler ()]
errHandlers = [Handler (\(e :: KevinException) -> case e of
    LostClient -> klogError "Lost client connection, DCing server"
    x -> throwIO x)]