{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevin.Damn.Packet (
    Packet(..),
    command, parameter, args, body,

    parsePacket,
    parsePrivclasses,
    subPacket,
    fixLoginPacket,
    okay,
    readable,
    null_,
    notNull_
) where

import Control.Applicative (many, (<$>), (<$))
import Control.Exception (throw)
import Control.Lens
import Control.Monad (guard, liftM2)
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Kevin.Base (KevinException(..), Privclass)

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x = x <$ guard (f x)

notNull_ :: Prism' T.Text T.Text
notNull_ = prism' id $ toMaybe (not . T.null)

null_ :: Prism' T.Text T.Text
null_ = prism' id $ toMaybe T.null

data Packet = Packet { _command     :: T.Text
                     , _parameter   :: Maybe T.Text
                     , _args        :: M.Map T.Text T.Text
                     , _body        :: Maybe T.Text
                     } deriving Show

makeLenses ''Packet

parseCommand :: Parser T.Text
parseCommand = takeWhile1 (not . isSpace)

parseParam :: Parser (Maybe T.Text)
parseParam = do char ' '
                parm <- takeWhile1 (not . isSpace)
                return $ Just parm

parseArgs :: Parser (M.Map T.Text T.Text)
parseArgs = (M.fromList <$>) . many $ do char '\n'
                                         c <- takeTill (=='=')
                                         char '='
                                         r <- takeTill (=='\n')
                                         return (c,r)

parseHead :: Parser Packet
parseHead = do c <- parseCommand
               p <- option Nothing parseParam
               a <- parseArgs
               return $ Packet c p a Nothing

parsePacket :: T.Text -> Packet
parsePacket pack = case parseOnly parseHead top of Left _    -> throw ParseFailure
                                                   Right res -> res & body .~ (T.drop 2 <$> toMaybe (not . T.null) b)
    where (top, b) = T.breakOn "\n\n" pack

getResult :: Either String a -> a
getResult x = let Right e = x in e

fixLoginPacket :: Packet -> Packet
fixLoginPacket pkt = if pkt^.command == "login"
                        then pkt & args %~ (<> getResult (parseOnly parseArgs . T.cons '\n' . fromJust $ pkt^.body))
                        else pkt

subPacket :: Packet -> Maybe Packet
subPacket = (parsePacket <$>) . view body

okay :: Packet -> Bool
okay (Packet _ _ a _) = let e = a ^. at "e" in isNothing e || e == Just "ok"

parsePrivclasses :: T.Text -> [Privclass]
parsePrivclasses = map (liftM2 (,) (!! 1) (read . T.unpack . head) . T.splitOn ":")
                 . filter (not . T.null)
                 . T.splitOn "\n"

readable :: Packet -> T.Text
readable (Packet cmd param arg bod) = cmd <> maybe "" (' ' `T.cons`) param
                                          <> formattedArgs (M.toList arg)
                                          <> maybe "" ("\n\n" <>) bod
                                          <> "\n\0"
    where
        formattedArgs [] = ""
        formattedArgs q  = ("\n" <>) . T.intercalate "\n" . map (uncurry (\x y -> x <> "=" <> y)) $ q
