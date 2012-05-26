module Kevin.Util.Token (
    getToken
) where

import Network.Curl
import Network.HTTP
import Data.List (isInfixOf, intercalate)
import qualified Data.ByteString.Char8 as B

type Headers = [(String,String)]

getToken :: B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
getToken u pass = withCurlDo $ do
    curl <- initialize
    -- thanks for not having any accessors, Network.Curl!
    (CurlResponse _ _ _ (headers :: Headers) (_ :: String) _) <-
        do_curl_ curl
                 "https://www.deviantart.com/users/login" 
                 [CurlPostFields
                     ["username=" ++ urlEncode (B.unpack u)
                     ,"password=" ++ urlEncode (B.unpack pass)
                     ]
                 ]
    case lookup "Location" headers of
        Just st -> if "wrong-password" `isInfixOf` st
            then return Nothing
            else do
                let cookies = map (takeWhile (/=';') . tail . snd) $ filter (("Set-Cookie" ==) . fst) headers
                (CurlResponse _ _ _ (_ :: Headers) (body :: B.ByteString) _) <-
                    do_curl_ curl
                             "https://chat.deviantart.com/chat/WriteRoom"
                             [CurlCookie $ intercalate ";" cookies]
                let chunk = snd $ B.breakSubstring "dAmn_Login(" body
                    token = B.takeWhile (/='"') $ B.tail $ B.dropWhile (/='\"') $ B.dropWhile (/=',') chunk
                return $ Just token
        Nothing -> return Nothing