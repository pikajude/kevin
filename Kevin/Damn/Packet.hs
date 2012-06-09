module Kevin.Damn.Packet (
    Packet(..),
    parsePacket,
    parsePrivclasses,
    subPacket,
    fixLoginPacket,
    okay,
    getArg,
    splitOn,
	readable
) where

import Kevin.Base (KevinException(..), Privclass)
import Control.Exception (throw)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Char
import Control.Applicative (many, (<$>))
import Control.Monad (liftM2)
import Control.Monad.Fix
import Data.Maybe

data Packet = Packet { command :: T.Text
                     , parameter :: Maybe T.Text
                     , args :: [(T.Text,T.Text)]
                     , body :: Maybe T.Text
                     } deriving (Show)

parseCommand :: Parser T.Text
parseCommand = takeWhile1 (not . isSpace)

parseParam :: Parser (Maybe T.Text)
parseParam = do
    char ' '
    parm <- takeWhile1 (not . isSpace)
    return $ Just parm

parseArgs :: Parser [(T.Text,T.Text)]
parseArgs = many $ do
    char '\n'
    c <- takeTill (=='=')
    char '='
    r <- takeTill (=='\n')
    return (c,r)

parseHead :: Parser Packet
parseHead = do
    c <- parseCommand
    p <- option Nothing parseParam
    a <- parseArgs
    return $ Packet c p a Nothing

parsePacket :: T.Text -> Packet
parsePacket pack = let (top,b) = T.breakOn "\n\n" pack in case (\x -> (x { body = if T.null b then Nothing else Just $ T.drop 2 b }) :: Packet) <$> parseOnly parseHead top of
    Left _ -> throw ParseFailure
    Right pkt -> pkt

getResult :: Either String a -> a
getResult (Right x) = x
getResult (Left _) = error "getResult"

fixLoginPacket :: Packet -> Packet
fixLoginPacket pkt = if command pkt == "login"
    then pkt { args = args pkt ++ getResult (parseOnly parseArgs $ T.cons '\n' $ fromJust $ body pkt), body = Nothing }
    else pkt

subPacket :: Packet -> Maybe Packet
subPacket = (parsePacket <$>) . body

okay :: Packet -> Bool
okay (Packet _ _ a _) = let e = lookup "e" a in isNothing e || e == Just "ok"

getArg :: T.Text -> Packet -> T.Text
getArg b p = fromMaybe "" $ lookup b (args p)

parsePrivclasses :: T.Text -> [Privclass]
parsePrivclasses = map (liftM2 (,) (!! 1) (read . T.unpack . (!! 0)) . T.splitOn ":") . filter (not . T.null) . T.splitOn "\n"

splitOn :: T.Text -> T.Text -> [T.Text]
splitOn delim = fix (\rec s -> let (f,l) = T.breakOn delim s in if T.null l then [f] else f:rec (T.drop (T.length delim) l))

readable :: Packet -> T.Text
readable (Packet cmd param arg bod) = cmd +++ maybe "" (' ' `T.cons`) param +++ formattedArgs arg +++ maybe "" ("\n\n" `T.append`) bod +++ "\n\0"
    where
        (+++) = T.append
        formattedArgs [] = ""
        formattedArgs q = T.append "\n" $ T.intercalate "\n" $ map (uncurry (\x y -> x +++ "=" +++ y)) q