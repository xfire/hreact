import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (newTVar, TVar, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import Control.Monad (when)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Time (getCurrentTime)
import Prelude hiding (filter)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.FilePath.Find
import System.FilePath.GlobPattern ((~~))
import System.INotify
import System.IO
import System.Process
import Text.Regex.Posix ((=~))

data State = State {
  stRunning :: Bool,
  stCounter :: Int
  } 

data OptError = OptError Int String
 
data Options = Options {
      optPath :: FilePath
    , optCommand :: String
    , optRegex :: Maybe String
    , optGlobPattern :: Maybe String
    , optHelp :: Bool
    , optVersion :: Bool
} deriving Show

defaultOptions :: Options
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
        state <- atomically $ newTVar $ State False 0
        inotify <- initINotify
        dirs <- getAllDirectories
        mapM_ (watchSingle inotify state) (path : dirs)
        putStrLn "Listens to your home directory. Hit enter to terminate."
        loop cmd state
        {- removeWatch inotify wd -}
    where path = optPath opts
          cmd = optCommand opts
          filter = filterPath opts

          getAllDirectories :: IO [FilePath]
          getAllDirectories = find always (fileType ==? Directory) path

          watchSingle :: INotify -> TVar State -> FilePath -> IO WatchDescriptor
          watchSingle inotify state path' = addWatch inotify [ Create, Delete, Modify, Move ] path' $ handleEvent cmd state filter path'

loop :: String -> TVar State -> IO ()
loop cmd state = do
    key <- getChar
    when ((ord key) == 10) $ do
        executeAction cmd state Nothing
        loop cmd state
    when (key == 'q') $ exitWith ExitSuccess

handleEvent :: String -> TVar State -> (FilePath -> Bool) -> FilePath -> Event -> IO ()
handleEvent cmd state filter path event = do
    when (shouldExecute event filter) $ executeAction cmd state $ getFile event >>= \f -> return (path </> f)

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

substitute :: String -> String -> String
substitute p ('\\':'$':'f':rest) = "\\$f" ++ substitute p rest
substitute p ('$':'f':rest) = p ++ substitute p rest
substitute p (x:rest) = x : substitute p rest
substitute _ [] = []

executeAction :: String -> TVar State -> Maybe FilePath -> IO ()
executeAction cmd state path = fork action
  where
  fork a = forkIO a >> return ()
  cmd' = case path of
    Nothing -> substitute "<<unknown>>" cmd
    Just p -> substitute p cmd

  action = do
    continue <- atomically $ do 
      s <- readTVar state
      if stRunning s
        then writeTVar state (s {stCounter = stCounter s + 1}) >> return False
        else writeTVar state (s {stRunning = True}) >> return True
    when continue $ do
      _ <- system cmd'
      putStr $ take 20 $ repeat '-'
      putStr " "
      time <- getCurrentTime
      putStr $ show time
      counter <- atomically $ do
        s <- readTVar state
        writeTVar state (s {stRunning = False, stCounter = 0})
        return $ stCounter s
      when (counter > 0) $ putStr $ " (" ++ show counter ++ ")"
      putStrLn ""

filterPath :: Options -> FilePath -> Bool
filterPath o path = or $ [filterRegex $ optRegex o, filterGlob $ optGlobPattern o] 
    where filterRegex :: Maybe String -> Bool
          filterRegex (Just regex) = path =~ regex
          filterRegex _ = False

          filterGlob :: Maybe String -> Bool
          filterGlob (Just glob) = path ~~ glob
          filterGlob _ = False

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    prg <- getProgName
    opts <- exitOnError $ parseArgs prg args
    watch opts

exitOnError :: Either OptError a -> IO a
exitOnError (Left (OptError code msg)) = hPutStrLn stderr (msg) >> exitWith (ExitFailure code)
exitOnError (Right x) = return x

