module Kevin.Util.Token (
    getToken
) where

import Network.TLS
import Network.TLS.Extra
import Network.HTTP.Base
import Crypto.Random.AESCtr (makeSystem)
import Control.Arrow
import Control.Monad (guard)
import Data.List
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

recvUntil :: TLSCtx a -> B.ByteString -> IO B.ByteString
recvUntil ctx str = do
    line <- recvData ctx
    if str `B.isSuffixOf` line
        then return str
        else fmap (line `B.append`) $ recvUntil ctx str

concatHeaders :: [(String,String)] -> String
concatHeaders = intercalate "\r\n" . map (\(x,y) -> x ++ ": " ++ y)

getToken :: T.Text -> T.Text -> IO (Maybe T.Text)
getToken uname pass = do
    let params = defaultParams { pCiphers = ciphersuite_all
                               , onCertificatesRecv = certificateChecks
                                     [certificateVerifyChain,
                                      return . certificateVerifyDomain "chat.deviantart.com"]
                               }
        headers = [("Connection", "closed"),
                   ("Content-Type", "application/x-www-form-urlencoded")] :: [(String,String)]
    gen <- makeSystem
    ctx <- connectionClient "www.deviantart.com" "443" params gen
    let payload = urlEncodeVars [("username", T.unpack uname),("password", T.unpack pass),("remember_me","1")]

    handshake ctx
    sendData ctx . LB.pack $ printf "POST /users/login HTTP/1.1\r\n%s\r\nContent-Length: %d\r\n\r\n%s" (concatHeaders $ ("Host", "www.deviantart.com"):headers) (length payload) payload

    bs <- recvData ctx

    guard . not $ "wrong-password" `B.isInfixOf` bs

    let cookie = B.intercalate ";" . map snd . filter ((== "Set-Cookie") . fst) . map (second (B.drop 2 . B.takeWhile (/=';')) . B.breakSubstring ": ") . B.lines $ bs
        s = printf "GET /chat/Botdom HTTP/1.1\r\n%s\r\ncookie: %s\r\n\r\n" (concatHeaders [("Host", "chat.deviantart.com")]) (B.unpack cookie)

    print s
    sendData ctx $ LB.pack s

    bq <- recvUntil ctx "</html>\n\r\n0\r\n\r\n"
    return . Just . decodeUtf8 . B.take 32 . B.tail . B.dropWhile (/='"') . B.dropWhile (/=',') . snd $ B.breakSubstring "dAmn_Login" bq