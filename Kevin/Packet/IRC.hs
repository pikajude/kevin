module Kevin.Packet.IRC (
    Packet(..),
    parsePacket
) where

import Kevin.Base (KevinException(..), KClientPacket(..))
import Prelude hiding (takeWhile)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Attoparsec.ByteString.Char8
import Control.Exception (throw)
import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))

data Packet = Packet { prefix :: Maybe B.ByteString
                     , command :: B.ByteString
                     , params :: [B.ByteString]
                     } deriving (Show)

badChars :: String
badChars = "\x20\x0\xd\xa"

spaces :: Parser B.ByteString
spaces = takeWhile1 isSpace

servername :: Parser B.ByteString
servername = takeWhile1 (inClass "a-z0-9.-")

username :: Parser B.ByteString
username = do
    n <- nick
    u <- option "" (B.cons <$> char '!' <*> user)
    h <- option "" (B.cons <$> char '@' <*> servername)
    return $ B.concat [n, u, h]
    
nick :: Parser B.ByteString
nick = B.cons <$> letter_ascii <*> takeWhile (inClass "a-zA-Z0-9[]\\`^{}-")

user :: Parser B.ByteString
user = takeWhile1 (notInClass badChars)

parsePrefix :: Parser B.ByteString
parsePrefix = username <|> servername

parseCommand :: Parser B.ByteString
parseCommand = takeWhile1 isAlpha_ascii <|>
               (do { a <- digit; b <- digit; c <- digit; return $ B.pack [a,b,c]})

parseParams :: Parser [B.ByteString]
parseParams = (colonParam <|> nonColonParam) `sepBy` spaces

colonParam :: Parser B.ByteString
colonParam = char ':' *> takeWhile (notInClass "\x0\xd\xa")

nonColonParam :: Parser B.ByteString
nonColonParam = takeWhile (notInClass badChars)

crlf :: Parser B.ByteString
crlf = string "\r\n"

messageBegin :: Parser (Maybe B.ByteString)
messageBegin = Just <$> (char ':' *> parsePrefix <* spaces)

packetParser :: Parser Packet
packetParser = do
    pre <- option Nothing messageBegin
    cmd <- B.map toUpper <$> parseCommand
    spaces
    par <- filter (not . B.null) <$> parseParams
    option "" crlf
    return $ Packet pre cmd par

parsePacket :: B.ByteString -> Packet
parsePacket str = case parseOnly packetParser str of
   Left _ -> throw ParseFailure
   Right p -> p

showParams :: [B.ByteString] -> B.ByteString
showParams = B.unwords . map (\str -> if ' ' `B.elem` str then B.cons ':' str else str)

instance KClientPacket Packet where
    asStringC (Packet (Just str) cmd pms) = flip B.append "\r\n" $ B.unwords [B.cons ':' str, cmd, showParams pms]
    asStringC (Packet Nothing c p) = flip B.append "\r\n" $ B.unwords [c, showParams p]