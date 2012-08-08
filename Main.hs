import Kevin
import System.Console.GetOpt
import System.Environment

defaultPort :: Int
defaultPort = 6669

data Flag = Port Int | Version | Help deriving (Eq)

opts :: [OptDescr Flag]
opts = [
        Option "p" ["port"] (ReqArg (Port . read) "number") $ "local port to run the server on (defaults to " ++ show defaultPort ++ ")",
        Option "h" ["help"] (NoArg Help) "print this message",
        Option "v" ["version"] (NoArg Version) "show kevin's version number"
       ]

header :: String
header = "Usage: kevin [options...]"

getPort :: [Flag] -> Int
getPort (Port x:_) = x
getPort (_:xs) = getPort xs
getPort [] = defaultPort

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute opts args of
        (flags, _, []) -> case flags of
            f | Help `elem` f -> putStrLn $ usageInfo header opts
              | Version `elem` f -> putStrLn $ "kevin version " ++ VERSION
              | otherwise -> kevinServer $ getPort flags
        (_, _, msgs@(_:_)) -> putStrLn $ concat msgs ++ usageInfo header opts
