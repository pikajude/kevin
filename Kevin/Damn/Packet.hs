module Kevin.Damn.Packet (
    Packet(..),
    parsePacket,
    parsePrivclasses,
    subPacket,
    fixLoginPacket,
    okay,
    getArg,
    splitOn
) where

import Kevin.Base (KevinException(..), KServerPacket(..), Privclass)
import Control.Exception (throw)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative (many, (<$>))
import Control.Monad (liftM2)
import Data.Maybe

data Packet = Packet { command :: B.ByteString
                     , parameter :: Maybe B.ByteString
                     , args :: [(B.ByteString,B.ByteString)]
                     , body :: Maybe B.ByteString
                     } deriving (Show)

parseCommand :: Parser B.ByteString
parseCommand = takeWhile1 (not . isSpace)

parseParam :: Parser (Maybe B.ByteString)
parseParam = do
    char ' '
    parm <- takeWhile1 (not . isSpace)
    return $ Just parm

parseArgs :: Parser [(B.ByteString,B.ByteString)]
parseArgs = many $ do
    char '\n'
    c <- takeTill (=='=')
    char '='
    r <- takeTill isSpace
    return (c,r)

parseHead :: Parser Packet
parseHead = do
    c <- parseCommand
    p <- option Nothing parseParam
    a <- parseArgs
    return $ Packet c p a Nothing

parsePacket :: B.ByteString -> Packet
parsePacket pack = let (top,b) = B.breakSubstring "\n\n" pack in case fmap (\x -> (x { body = if B.null b then Nothing else Just $ B.drop 2 b }) :: Packet) $ parseOnly parseHead top of
    Left _ -> throw ParseFailure
    Right pkt -> pkt

getResult :: Either String a -> a
getResult (Right x) = x
getResult (Left _) = error "getResult"

fixLoginPacket :: Packet -> Packet
fixLoginPacket pkt = if command pkt == "login"
    then pkt { args = args pkt ++ getResult (parseOnly parseArgs $ B.cons '\n' $ fromJust $ body pkt), body = Nothing }
    else pkt

subPacket :: Packet -> Maybe Packet
subPacket = (parsePacket <$>) . body

okay :: Packet -> Bool
okay (Packet _ _ a _) = let e = lookup "e" a in isNothing e || e == Just "ok"

getArg :: B.ByteString -> Packet -> B.ByteString
getArg b p = maybe "" id $ lookup b (args p)

parsePrivclasses :: B.ByteString -> [Privclass]
parsePrivclasses = map (liftM2 (,) (!! 0) (read . B.unpack . (!! 1)) . B.split ':') . B.split '\n'

splitOn :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn delim str = if B.null l then [f] else f:splitOn delim (B.drop (B.length delim) l)
    where
        (f,l) = B.breakSubstring delim str

instance KServerPacket Packet where
    asStringS (Packet cmd param arg bod) = cmd +++ maybe "" (' ' `B.cons`) param +++ formattedArgs arg +++ maybe "" ("\n\n" `B.append`) bod +++ "\n\0"
        where
            (+++) = B.append
            formattedArgs [] = ""
            formattedArgs q = B.append "\n" $ B.intercalate "\n" $ map (uncurry (\x y -> x +++ "=" +++ y)) q