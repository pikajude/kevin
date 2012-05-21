{-# LANGUAGE OverloadedStrings #-}

module Kevin.Protocol.Client (initialize, cleanup) where

import Kevin.Util.Logger
import System.IO

initialize :: Handle -> IO ()
initialize = const $ klog Green "initialize client"

cleanup :: Handle -> IO ()
cleanup = const $ klog Green "cleanup client"