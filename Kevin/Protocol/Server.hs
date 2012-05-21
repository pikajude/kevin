{-# LANGUAGE OverloadedStrings #-}

module Kevin.Protocol.Server (initialize, cleanup) where

import Data.ByteString.Char8
import Kevin.Util.Logger
import System.IO

initialize :: Handle -> IO ()
initialize h = hPut h "dAmnClient 0.3\nagent=kevin 0.1\n\0"

cleanup :: Handle -> IO ()
cleanup = const $ klog Blue "cleanup server"