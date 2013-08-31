{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevin.IRC.Packet (
    Packet(..),
    prefix, command, params,

    parsePacket,
    readable
) where

import           Control.Applicative  ((<|>), (<$>), (<*>), (*>), (<*))
import           Control.Exception    (throw)
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Monoid
import qualified Data.Text as T
import           Kevin.Base           (KevinException(..))
import           Prelude hiding       (takeWhile)

data Packet = Packet { _prefix  :: Maybe T.Text
                     , _command :: T.Text
                     , _params  :: [T.Text]
                     } deriving (Show)


makeLenses ''Packet

badChars :: String
badChars = "\x20\x0\xd\xa"

spaces :: Parser T.Text
spaces = takeWhile1 isSpace

servername :: Parser T.Text
servername = takeWhile1 (inClass "a-z0-9.-")

username :: Parser T.Text
username = do
    n <- nick
    u <- option "" (T.cons <$> char '!' <*> user)
    h <- option "" (T.cons <$> char '@' <*> servername)
    return $ T.concat [n, u, h]

nick :: Parser T.Text
nick = T.cons <$> letter <*> takeWhile (inClass "a-zA-Z0-9[]\\`^{}-")

user :: Parser T.Text
user = takeWhile1 (notInClass badChars)

parsePrefix :: Parser T.Text
parsePrefix = username <|> servername

parseCommand :: Parser T.Text
parseCommand = takeWhile1 isAlpha <|> liftM3 (\a b c -> T.pack [a,b,c]) digit digit digit

parseParams :: Parser [T.Text]
parseParams = (colonParam <|> nonColonParam) `sepBy` spaces

colonParam :: Parser T.Text
colonParam = char ':' *> takeWhile (notInClass "\x0\xd\xa")

nonColonParam :: Parser T.Text
nonColonParam = takeWhile (notInClass badChars)

crlf :: Parser T.Text
crlf = string "\r\n"

messageBegin :: Parser (Maybe T.Text)
messageBegin = Just <$> (char ':' *> parsePrefix <* spaces)

packetParser :: Parser Packet
packetParser = do
    pr <- option Nothing messageBegin
    cmd <- T.map toUpper <$> parseCommand
    spaces
    par <- filter (not . T.null) <$> parseParams
    option "" crlf
    return $ Packet pr cmd par

parsePacket :: T.Text -> Packet
parsePacket str = case parseOnly packetParser str of
                      Left err -> throw $ ClientParseFailure err str
                      Right p  -> p

showParams :: [T.Text] -> T.Text
showParams = T.unwords . map (\str ->
                 if " " `T.isInfixOf` str
                     then T.cons ':' str
                     else str)

readable :: Packet -> T.Text
readable (Packet (Just str) cmd pms) = (<> "\r\n") $ T.unwords [T.cons ':' str, cmd, showParams pms]
readable (Packet Nothing c p)        = (<> "\r\n") $ T.unwords [c, showParams p]
