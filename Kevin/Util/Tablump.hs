{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Kevin.Util.Tablump (
    tablumpDecode
) where

import Text.Regex.PCRE
import Text.Regex.PCRE.String
import Text.Printf
import Control.Arrow
import Control.Monad.Fix
import System.IO.Unsafe
import qualified Data.Text as T

fromRight :: (Show a) => Either a b -> b
fromRight (Left x) = error $ "fromRight on Left " ++ show x
fromRight (Right a) = a

{-# NOINLINE regexReplace #-}
regexReplace :: Regex -> ([String] -> String) -> String -> String
regexReplace find replace = fix (\f str -> case fromRight . unsafePerformIO $ regexec find str of
    Just (bef, _, af, matches) -> concat [bef, replace matches, f af]
    Nothing -> str)

{-# NOINLINE regexen #-}
regexen :: [(Regex, [String] -> String)]
regexen = let ($$) = (,) in map (first (fromRight . unsafePerformIO . compile defaultCompOpt defaultExecOpt)) . reverse $ [
        "&b\t"      $$ const "\2",
        "&/b\t"     $$ const "\15",
        "&i\t"      $$ const "\22",
        "&/i\t"     $$ const "\15",
        "&u\t"      $$ const "\31",
        "&/u\t"     $$ const "\15",
        "&s\t"      $$ const "<s>",
        "&/s\t"     $$ const "</s>",
        "&sup\t"    $$ const "",
        "&/sup\t"   $$ const "",
        "&sub\t"    $$ const "",
        "&/sub\t"   $$ const "",
        "&code\t"   $$ const "",
        "&/code\t"  $$ const "",
        "&br\t"     $$ const "\n",
        "&ul\t"     $$ const "",
        "&/ul\t"    $$ const "",
        "&ol\t"     $$ const "",
        "&/ol\t"    $$ const "",
        "&li\t"     $$ const "- ",
        "&/li\t"    $$ const "\n",
        "&bcode\t"  $$ const "",
        "&/bcode\t" $$ const "",
        "&/a\t"     $$ const ")",
        "&/acro\t"  $$ const "</acronym>",
        "&/abbr\t"  $$ const "</abbr>",
        "&p\t"      $$ const "",
        "&/p\t"     $$ const "\n",
        "&emote\t(.+?)\t.+?\t.+?\t.+?\t.+?\t" $$ head,
        "&a\t(.+?)\t.*?\t" $$ \(x:_) -> printf "%s (" x,
        "&link\t(.+?)\t&\t" $$ head,
        "&link\t(.+?)\t(.+?)\t&\t" $$ \(x:y:_) -> printf "%s (%s)" x y,
        "&dev\t.+?\t(.+?)\t" $$ head,
        "&avatar\t(.+?)\t.+?\t" $$ \(x:_) -> printf ":icon%s:" x,
        "&thumb\t(.+?)\t.+?\t.+?\t.+?\t.+?\t.+?\t.+?\t" $$ \(x:_) -> printf ":thumb%s:" x,
        "&img\t(.+?)\t(.*?)\t(.*?)\t" $$ \(x:y:z:_) -> printf "<img src='%s' alt='%s' title='%s' />" x y z,
        "&iframe\t(.+?)\t(.*?)\t(.*?)\t" $$ \(x:y:z:_) -> printf "<iframe src='%s' width='%s' height='%s' />" x y z,
        "&acro\t(.+?)\t" $$ \(x:_) -> printf "<acronym title='%s'>" x,
        "&abbr\t(.+?)\t" $$ \(x:_) -> printf "<abbr title='%s'>" x,
        " <abbr title='colors:[0-9A-Fa-f]{6}:[0-9A-Fa-f]{6}'></abbr>" $$ const "",
        "^<abbr title='(.+?)'>.+?</abbr>:" $$ \(x:_) -> printf "%s:" x,
        "^[a-zA-Z0-9\\-_]+<abbr title='(.+?)'></abbr>:" $$ \(x:_) -> printf "%s:" x
    ]

tablumpDecode :: T.Text -> T.Text
tablumpDecode = T.pack . flip (foldr (uncurry regexReplace)) regexen . T.unpack
