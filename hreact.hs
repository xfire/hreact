import System.IO
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import System.Console.GetOpt
import System.Process
import Data.Char (ord)
import Control.Monad (when)
import Text.Regex.Posix ((=~))
import System.INotify
import System.Time
import System.Locale
import System.Environment

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
    , Option ['g'] ["glob"]     (OptArg glob "GLOB") "Glob Pattern"
    , Option ['V'] ["version"]  (NoArg $ const $ showVersion >> exitWithSuccess) "Print version"
    , Option ['h'] ["help"]     (NoArg $ const $ showHelp >> exitWithSuccess) "Show help"
    ]

regex arg opt = return opt { optRegex = arg }
glob arg opt = return opt { optGlob = arg }

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

watch :: FilePath -> String -> (FilePath -> Bool) -> IO ()
watch path cmd filter = do
    inotify <- initINotify
    wd <- addWatch inotify [ Create, Delete, Modify, Move ] path $ handleEvent cmd filter
    putStrLn "Listens to your home directory. Hit enter to terminate."
    loop cmd
    removeWatch inotify wd

loop :: String -> IO ()
loop cmd = do
    key <- getChar
    when ((ord key) == 10) $ do
        executeAction cmd
        loop cmd

handleEvent :: String -> (FilePath -> Bool) -> Event -> IO ()
handleEvent cmd filter event = do
    when (shouldExecute event filter) $ executeAction cmd

shouldExecute :: Event -> (FilePath -> Bool) -> Bool
shouldExecute (Opened False (Just path)) f = f path
shouldExecute (Closed False (Just path) _) f = f path
shouldExecute (Created False path) f = f path
shouldExecute (Deleted False path) f = f path
shouldExecute (Modified False (Just path)) f = f path
shouldExecute (MovedIn False path _) f = f path
shouldExecute (MovedOut False path _) f = f path
shouldExecute (MovedSelf _) _ = True
shouldExecute _ _ = False

executeAction :: String -> IO ()
executeAction cmd = do
    system cmd
    putStr $ take 20 $ repeat '-'
    putStr " "
    time <- getClockTime >>= toCalendarTime
    putStrLn $ formatCalendarTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" time

filterPath :: Options -> FilePath -> Bool
filterPath o path = or $ fmap ($ path) [filterRegex $ optRegex o, filterGlob $ optGlob o] 
    where filterRegex :: Maybe String -> FilePath -> Bool
          filterRegex (Just regex) path = path =~ regex
          filterRegex _ _ = True

          filterGlob :: Maybe String -> FilePath -> Bool
          filterGlob (Just glob) path = False
          filterGlob _ _ = False

main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    (opts, n) <- parseArgs args
    case n of
        (path:command:_) -> watch path command (filterPath opts)
        [_] -> exitErrors ["please specify the command", ""]
        _ -> exitErrors ["please specify path and command", ""]

