module Kevin (kevinServer) where
import Kevin.Base
import Kevin.Protocol

kevinServer :: IO ()
kevinServer = do
    sock <- mkListener
    forever $ do
        kev <- mkKevin sock
        runReaderT listen kev