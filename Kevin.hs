module Kevin (kevinServer) where
import Kevin.Base
import Kevin.Protocol
import Control.Monad (forever)

kevinServer :: IO ()
kevinServer = do
    sock <- mkListener
    forever $ do
        kev <- mkKevin sock
        listen kev