module Kevin.Util.Token (
    getToken
) where

import Control.Arrow
import Crypto.Random.AESCtr (makeSystem)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Base
import Network.TLS
import Network.TLS.Extra
import Text.Printf

recvUntil :: TLSCtx -> B.ByteString -> IO B.ByteString
recvUntil ctx str = do line <- recvData ctx
                       if str `B.isInfixOf` line
                          then return line
                          else fmap (line <>) $ recvUntil ctx str

concatHeaders :: [(String,String)] -> String
concatHeaders = intercalate "\r\n" . map (\(x,y) -> x ++ ": " ++ y)

scrapeFormValue :: String -> B.ByteString -> B.ByteString
scrapeFormValue key bs = B.takeWhile (/='"') . B.drop 7 . head
                       . filter (\l -> "value=" `B.isPrefixOf` l) . B.words . head
                       . filter (\l -> B.pack key `B.isInfixOf` l) . B.lines $ bs

scrapeCookies :: B.ByteString -> B.ByteString
scrapeCookies bs = B.intercalate ";" . map snd
                 . filter ((== "Set-Cookie") . fst)
                 . map (second (B.drop 2 . B.takeWhile (/=';'))
                       . B.breakSubstring ": ")
                 . B.lines $ bs

getToken :: T.Text -> T.Text -> IO (Maybe T.Text)
getToken uname pass = do let params = defaultParams { pCiphers = ciphersuite_all
                                                    , onCertificatesRecv = certificateChecks [return . certificateVerifyDomain "chat.deviantart.com"] }
                             headers :: [(String, String)]
                             headers = [("Connection", "closed"), ("Content-Type", "application/x-www-form-urlencoded")]
                         gen <- makeSystem
                         ctv <- connectionClient "www.deviantart.com" "443" params gen
                         handshake ctv
                         sendData ctv . LB.pack $ "GET /users/login HTTP/1.1\r\nHost: www.deviantart.com\r\n\r\n"
                         bl <- recvUntil ctv "validate_key"
                         bye ctv

                         let payload = urlEncodeVars [ ("username", T.unpack uname)
                                                     , ("password", T.unpack pass)
                                                     , ("validate_token", B.unpack (scrapeFormValue "validate_token" bl))
                                                     , ("validate_key", B.unpack (scrapeFormValue "validate_key" bl))
                                                     , ("remember_me","1")
                                                     ]
                         ctx <- connectionClient "www.deviantart.com" "443" params gen
                         handshake ctx
                         sendData ctx . LB.pack $ printf "POST /users/login HTTP/1.1\r\n%s\r\n\
                                                         \cookie: %s\r\nContent-Length: %d\r\n\r\n%s"
                                                         (concatHeaders $ ("Host", "www.deviantart.com"):headers)
                                                         (B.unpack (scrapeCookies bl))
                                                         (length payload)
                                                         payload
                         bs <- recvData ctx
                         if "wrong-password" `B.isInfixOf` bs
                            then return Nothing
                            else do let s = printf "GET /chat/Botdom HTTP/1.1\r\n%s\r\n\
                                                \cookie: %s\r\n\r\n"
                                                (concatHeaders [("Host", "chat.deviantart.com")])
                                                (B.unpack (scrapeCookies bs))
                                    sendData ctx $ LB.pack s
                                    bq <- recvUntil ctx "dAmnChat_Init"
                                    return . (Just . decodeUtf8 . B.take 32 . B.tail
                                             . B.dropWhile (/='"') . B.dropWhile (/=',') . snd)
                                           . B.breakSubstring "dAmn_Login" $ bq
