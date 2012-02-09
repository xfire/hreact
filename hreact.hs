import System.IO
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import System.Console.GetOpt
import Control.Monad (when)

data Options = Options
    { optRegex :: Maybe String
    , optGlob :: Maybe String
    } deriving Show
defaultOptions = Options
    { optRegex = Nothing
    , optGlob = Nothing
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['r'] ["regex"]    (OptArg regex "REGEX") "Regex"
    , Option ['g'] ["glob"]     (OptArg regex "GLOB") "Glob Pattern"
    , Option ['V'] ["version"]  (NoArg $ const $ showVersion >> exitWithSuccess) "Print version"
    , Option ['h'] ["help"]     (NoArg $ const $ showHelp >> exitWithSuccess) "Show help"
    ]

regex arg opt = return opt { optRegex = arg }

showVersion :: IO ()
showVersion = do
    hPutStrLn stderr "Version 0.01"

showHelp :: IO ()
showHelp = do
    prg <- getProgName
    let header = prg ++ " [OPTIONS] path command"
    hPutStrLn stderr ((usageInfo header options))
    hFlush stderr

exitWithSuccess :: IO a
exitWithSuccess = exitWith ExitSuccess

exitErrors :: [String] -> IO ()
exitErrors errors = do
    mapM_ (hPutStrLn stderr) errors
    showHelp
    exitWith $ ExitFailure 1

parseArgs :: [String] -> IO (Options, [String])
parseArgs args = do
    let ( actions, nonOpts, errors ) = getOpt RequireOrder options args
    when (not (null errors)) $ exitErrors errors
    opts <- foldl (>>=) (return defaultOptions) actions
    return (opts, nonOpts)

main = do
    args <- getArgs
    (opts, n) <- parseArgs args
    let Options { optRegex = regex } = opts
    case n of
        (path:command:_) -> print $ path ++ command
        [_] -> exitErrors ["please specify the command", ""]
        _ -> exitErrors ["please specify path and command", ""]
