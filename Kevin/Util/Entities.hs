module Kevin.Util.Entities (
    entityEncode,
    entityDecode
) where

import Text.HTML.TagSoup.Entity
import Codec.Binary.UTF8.String (encode, encodeString)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative ((<|>), (<$>))

decodeCharacter :: Parser B.ByteString
decodeCharacter = entityNumeric <|> entityNamed <|> Data.Attoparsec.ByteString.Char8.take 1

entityNumeric :: Parser B.ByteString
entityNumeric = do
    string "&#"
    entity <- takeWhile1 (inClass "xa-fA-F0-9")
    char ';'
    return $ maybe "?" (B.pack . encodeString . return) $ lookupNumericEntity $ B.unpack entity

entityNamed :: Parser B.ByteString
entityNamed = do
    char '&'
    entity <- takeWhile1 isAlpha_ascii
    char ';'
    return $ maybe "?" B.singleton $ lookupNamedEntity $ B.unpack entity

decodeParser :: Parser B.ByteString
decodeParser = B.concat <$> many1 decodeCharacter

entityDecode :: B.ByteString -> B.ByteString
entityDecode str = case parseOnly decodeParser str of
    Left str -> error str
    Right s -> s

entityEncode :: String -> 