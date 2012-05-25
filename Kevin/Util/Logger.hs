module Kevin.Util.Logger (
    klog,
    klogError,
    klogWarn,
    Color(..)
) where

import Data.Char (isSpace)

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
klog c str = let d = colorAsNum c in putStrLn $ "\027[" ++ show d ++ "m" ++ rtrimmed ++ "\027[0m"
    where
        rtrimmed = reverse $ dropWhile (\x -> isSpace x || x == '\x0') $ reverse str

klogError, klogWarn :: String -> IO ()

klogError = klog Red . ("ERROR :: " ++)
klogWarn = klog Yellow . ("WARNING :: " ++)