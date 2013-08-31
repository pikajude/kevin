{-# LANGUAGE OverloadedStrings #-}

module Kevin.Util.Logger (
    klog,
    klog_,
    klogNow,
    klogError,
    klogWarn,
    Color(..),
    runLogger,
    printf
) where

import           Control.Concurrent
import           Control.Monad.State
import           Data.Char           (isSpace)
import qualified Data.Text as T
import           Kevin.Types

data Color = Red | Blue | Green | Cyan | Magenta | Yellow | Gray

colorAsNum :: Color -> Int
colorAsNum Red = 31
colorAsNum Green = 32
colorAsNum Yellow = 33
colorAsNum Blue = 34
colorAsNum Magenta = 35
colorAsNum Cyan = 36
colorAsNum Gray = 37

runLogger :: Chan String -> IO ()
runLogger ch = void . forkIO . forever $ readChan ch >>= putStrLn

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys

printf :: T.Text -> [T.Text] -> T.Text
printf str reps = T.concat $ interleave (T.splitOn "%s" str) reps

render :: Color -> String -> String
render col str = "\027["
              ++ show (colorAsNum col)
              ++ "m" ++ rtrimmed
              ++ "\027[0m"
    where
        rtrimmed = reverse . dropWhile (\x -> isSpace x || x == '\x0') . reverse $ str

klog_ :: Chan String -> Color -> String -> IO ()
klog_ ch col str = writeChan ch $ render col str

klogNow :: Color -> String -> IO ()
klogNow c s = putStrLn $ render c s

klog :: Color -> String -> KevinIO ()
klog c str = gets_ logger >>= \ch -> liftIO $ klog_ ch c str

klogError, klogWarn :: String -> KevinIO ()

klogError = klog Red . ("ERROR :: " ++)
klogWarn = klog Yellow . ("WARNING :: " ++)
