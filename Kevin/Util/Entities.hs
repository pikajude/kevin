module Kevin.Util.Entities (
    entityEncode,
    entityDecode
) where

import Text.HTML.TagSoup.Entity
import Codec.Binary.UTF8.String (encode, encodeString)
import qualified Data.Text as T
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative ((<|>), (<$>))

decodeCharacter :: Parser T.Text
decodeCharacter = entityNumeric <|> entityNamed <|> Data.Attoparsec.ByteString.Char8.take 1

entityNumeric :: Parser T.Text
entityNumeric = do
    string "&#"
    entity <- takeWhile1 (inClass "xa-fA-F0-9")
    char ';'
    return $ maybe "?" (T.pack . encodeString . return) $ lookupNumericEntity $ T.unpack entity

entityNamed :: Parser T.Text
entityNamed = do
    char '&'
    entity <- takeWhile1 isAlpha_ascii
    char ';'
    return $ maybe "?" T.singleton $ lookupNamedEntity $ T.unpack entity

decodeParser :: Parser T.Text
decodeParser = T.concat <$> many1 decodeCharacter

entityDecode :: T.Text -> T.Text
entityDecode str = case parseOnly decodeParser str of
    Left str -> error str
    Right s -> s

entityEncode :: String -> 