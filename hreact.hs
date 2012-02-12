import System.IO
import Data.List (intercalate)
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

data OptError = OptError Int String
 
data Options = Options {
      optPath :: FilePath
    , optCommand :: String
    , optRegex :: Maybe String
    , optGlobPattern :: Maybe String
    , optHelp :: Bool
    , optVersion :: Bool
} deriving Show

defaultOptions = Options {
      optPath = ""
    , optCommand = ""
    , optRegex = Nothing
    , optGlobPattern = Nothing
    , optHelp = False
    , optVersion = False
}

options :: [ OptDescr (Options -> Options) ]
options =
    [ Option ['r'] ["regex"]    (OptArg (\arg opt -> opt { optRegex = arg }) "REGEX") "regex"
    , Option ['p'] ["pattern"]  (OptArg (\arg opt -> opt { optGlobPattern = arg }) "PATTERN") "shell glob pattern"
    , Option ['V'] ["version"]  (NoArg $ \opt -> opt { optVersion = True} ) "print version"
    , Option ['h'] ["help"]     (NoArg $ \opt -> opt { optHelp = True } ) "show help"
    ]

version :: String
version = "hreact version 0.01"

usage :: String -> String
usage prg =
    let header = "usage: " ++ prg ++ " [OPTIONS] directory command"
    in usageInfo header options

parseArgs :: String -> [String] -> Either OptError Options
parseArgs prg args =
    case getOpt Permute options args of
        (o, no, []) -> checkOptions o no
        (_, _, err) -> Left $ OptError 1 $ intercalate "\n" (err ++ [use])
    where use = usage prg
          checkOptions opts noOpts
                | optHelp o    = Left $ OptError 1 $ use
                | optVersion o = Left $ OptError 1 $ version
                | otherwise    = x noOpts
            where o = foldl (flip id) defaultOptions opts
                  x (path:command:[]) = Right $ o { optPath = path, optCommand = command }
                  x _ = Left $ OptError 1 $ "please specify directory and command\n\n" ++ use

watch :: Options -> IO ()
watch opts = do
        inotify <- initINotify
        dirs <- getAllDirectories path
        mapM_ (watchSingle inotify cmd filter) (path : dirs)
        putStrLn "Listens to your home directory. Hit enter to terminate."
        loop cmd
        {- removeWatch inotify wd -}
    where path = optPath opts
          cmd = optCommand opts
          filter = filterPath opts

          getAllDirectories :: FilePath -> IO [FilePath]
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
    prg <- getProgName
    opts <- exitOnError $ parseArgs prg args
    watch opts

exitOnError :: Either OptError a -> IO a
exitOnError (Left (OptError code msg)) = hPutStrLn stderr (msg) >> exitWith (ExitFailure code)
exitOnError (Right x) = return x

