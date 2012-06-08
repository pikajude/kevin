module Kevin.Util.Token (
    getToken
) where

import Network.Curl
import Network.HTTP
import Data.List (isInfixOf, intercalate)
import qualified Data.Text as T

type Headers = [(String,String)]

getToken :: T.Text -> T.Text -> IO (Maybe T.Text)
getToken u pass = withCurlDo $ do
    curl <- initialize
    -- thanks for not having any accessors, Network.Curl!
    (CurlResponse _ _ _ (headers :: Headers) (_ :: String) _) <-
        do_curl_ curl
                 "https://www.deviantart.com/users/login" 
                 [CurlPostFields
                     ["username=" ++ urlEncode (T.unpack u)
                     ,"password=" ++ urlEncode (T.unpack pass)
                     ]
                 ]
    case lookup "Location" headers of
        Just st -> if "wrong-password" `isInfixOf` st
            then return Nothing
            else do
                let cookies = map (takeWhile (/=';') . tail . snd) $ filter (("Set-Cookie" ==) . fst) headers
                (CurlResponse _ _ _ (_ :: Headers) (body :: String) _) <-
                    do_curl_ curl
                             "https://chat.deviantart.com/chat/WriteRoom"
                             [CurlCookie $ intercalate ";" cookies]
                let chunk = snd $ T.breakOn "dAmn_Login(" $ T.pack body
                    token = T.takeWhile (/='"') $ T.tail $ T.dropWhile (/='\"') $ T.dropWhile (/=',') chunk
                return $ Just token
        Nothing -> return Nothing