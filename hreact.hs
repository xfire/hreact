import System.IO
import System.Environment (getArgs, getProgName)
import Data.Time (getCurrentTime)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import System.Console.GetOpt
import System.Process
import Data.Char (ord)
import Control.Monad (when)
import Text.Regex.Posix ((=~))
import System.INotify
import System.FilePath.GlobPattern ((~~))
import System.FilePath.Find
import System.FilePath ((</>))
import Control.Applicative ((<$>), (<*>))
 
data Options = Options
    { optRegex :: Maybe String
    , optGlobPattern :: Maybe String
    } deriving Show
defaultOptions = Options
    { optRegex = Nothing
    , optGlobPattern = Nothing
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['r'] ["regex"]    (OptArg regex "REGEX") "regex"
    , Option ['p'] ["pattern"]  (OptArg glob "PATTERN") "shell glob pattern"
    , Option ['V'] ["version"]  (NoArg $ const $ showVersion >> exitWithSuccess) "print version"
    , Option ['h'] ["help"]     (NoArg $ const $ showHelp >> exitWithSuccess) "show help"
    ]

regex arg opt = return opt { optRegex = arg }
glob arg opt = return opt { optGlobPattern = arg }

showVersion :: IO ()
showVersion = do
    hPutStrLn stderr "Version 0.01"

showHelp :: IO ()
showHelp = do
    prg <- getProgName
    let header = "usage: " ++ prg ++ " [OPTIONS] directory command"
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
        dirs <- getAllDirectories path
        mapM_ (watchSingle inotify cmd filter) (path : dirs)
        putStrLn "Listens to your home directory. Hit enter to terminate."
        loop cmd
        {- removeWatch inotify wd -}
    where getAllDirectories :: FilePath -> IO [FilePath]
          getAllDirectories path = find always (fileType ==? Directory) path

          watchSingle :: INotify -> String -> (FilePath -> Bool) -> FilePath -> IO WatchDescriptor
          watchSingle inotify cmd filter path = addWatch inotify [ Create, Delete, Modify, Move ] path $ handleEvent cmd filter path

loop :: String -> IO ()
loop cmd = do
    key <- getChar
    when ((ord key) == 10) $ do
        executeAction cmd Nothing
        loop cmd

handleEvent :: String -> (FilePath -> Bool) -> FilePath -> Event -> IO ()
handleEvent cmd filter path event = do
    when (shouldExecute event filter) $ executeAction cmd $ getFile event >>= \f -> return (path </> f)

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

getFile :: Event -> Maybe FilePath
getFile (Opened _ jpath) = jpath
getFile (Closed _ jpath _) = jpath
getFile (Created _ path) = Just path
getFile (Deleted _ path) = Just path
getFile (Modified _ jpath) = jpath
getFile (MovedIn _ path _) = Just path
getFile (MovedOut _ path _) = Just path
getFile _ = Nothing

executeAction :: String -> Maybe FilePath -> IO ()
executeAction cmd path = do
    system cmd'
    putStr $ take 20 $ repeat '-'
    putStr " "
    time <- getCurrentTime
    putStrLn $ show time

    where
      cmd' = case path of
        Nothing -> substitute "<<unknown>>" cmd
        Just p -> substitute p cmd
      substitute p ('\\':'$':'f':rest) = "\\$f" ++ substitute p rest
      substitute p ('$':'f':rest) = p ++ substitute p rest
      substitute p (x:rest) = x : substitute p rest
      substitute _ [] = []

filterPath :: Options -> FilePath -> Bool
filterPath o path = or $ fmap ($ path) [filterRegex $ optRegex o, filterGlob $ optGlobPattern o] 
    where filterRegex :: Maybe String -> FilePath -> Bool
          filterRegex (Just regex) path = path =~ regex
          filterRegex _ _ = False

          filterGlob :: Maybe String -> FilePath -> Bool
          filterGlob (Just glob) path = path ~~ glob
          filterGlob _ _ = False

main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    (opts, n) <- parseArgs args
    case n of
        (path:command:_) -> watch path command (filterPath opts)
        [_] -> exitErrors ["please specify the command", ""]
        _ -> exitErrors ["please specify path and command", ""]

