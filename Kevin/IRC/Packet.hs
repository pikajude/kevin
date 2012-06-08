module Kevin.IRC.Packet (
    Packet(..),
    parsePacket
) where

import Kevin.Base (KevinException(..), KClientPacket(..))
import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Char
import Data.Attoparsec.Text
import Control.Exception (throw)
import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))

data Packet = Packet { prefix ∷ Maybe T.Text
                     , command ∷ T.Text
                     , params ∷ [T.Text]
                     } deriving (Show)

badChars ∷ String
badChars = "\x20\x0\xd\xa"

spaces ∷ Parser T.Text
spaces = takeWhile1 isSpace

servername ∷ Parser T.Text
servername = takeWhile1 (inClass "a-z0-9.-")

username ∷ Parser T.Text
username = do
    n ← nick
    u ← option "" (T.cons <$> char '!' <*> user)
    h ← option "" (T.cons <$> char '@' <*> servername)
    return $ T.concat [n, u, h]
    
nick ∷ Parser T.Text
nick = T.cons <$> letter <*> takeWhile (inClass "a-zA-Z0-9[]\\`^{}-")

user ∷ Parser T.Text
user = takeWhile1 (notInClass badChars)

parsePrefix ∷ Parser T.Text
parsePrefix = username <|> servername

parseCommand ∷ Parser T.Text
parseCommand = takeWhile1 isAlpha <|>
               (do { a ← digit; b ← digit; c ← digit; return $ T.pack [a,b,c]})

parseParams ∷ Parser [T.Text]
parseParams = (colonParam <|> nonColonParam) `sepBy` spaces

colonParam ∷ Parser T.Text
colonParam = char ':' *> takeWhile (notInClass "\x0\xd\xa")

nonColonParam ∷ Parser T.Text
nonColonParam = takeWhile (notInClass badChars)

crlf ∷ Parser T.Text
crlf = string "\r\n"

messageBegin ∷ Parser (Maybe T.Text)
messageBegin = Just <$> (char ':' *> parsePrefix <* spaces)

packetParser ∷ Parser Packet
packetParser = do
    pre ← option Nothing messageBegin
    cmd ← T.map toUpper <$> parseCommand
    spaces
    par ← filter (not . T.null) <$> parseParams
    option "" crlf
    return $ Packet pre cmd par

parsePacket ∷ T.Text → Packet
parsePacket str = case parseOnly packetParser str of
   Left _ → throw ParseFailure
   Right p → p

showParams ∷ [T.Text] → T.Text
showParams = T.unwords . map (\str → if " " `T.isInfixOf` str then T.cons ':' str else str)

instance KClientPacket Packet where
    asStringC (Packet (Just str) cmd pms) = flip T.append "\r\n" $ T.unwords [T.cons ':' str, cmd, showParams pms]
    asStringC (Packet Nothing c p) = flip T.append "\r\n" $ T.unwords [c, showParams p]
