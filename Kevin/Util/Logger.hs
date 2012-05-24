module Kevin.Util.Logger (
    klog,
    klogError,
    klogWarn,
    Color(..)
) where

import Prelude hiding (log)
import Text.Printf

data Color = Red | Blue | Green | Cyan | Magenta | Yellow | Gray

colorAsNum :: Color -> Int
colorAsNum Red = 31
colorAsNum Green = 32
colorAsNum Yellow = 33
colorAsNum Blue = 34
colorAsNum Magenta = 35
colorAsNum Cyan = 36
colorAsNum Gray = 37

klog :: Color -> String -> IO ()
klog c str = let d = colorAsNum c in printf "\027[%dm%s\027[0m\n" d str

klogError, klogWarn :: String -> IO ()

klogError = klog Red . ("ERROR :: " ++)
klogWarn = klog Yellow . ("WARNING :: " ++)