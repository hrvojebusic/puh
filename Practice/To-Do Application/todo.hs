import System.Environment
import System.Directory
import System.IO
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No command was specified"
    (command:argList) -> dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
-- New actions should be inserted here.
dispatch command = commandNotSupported command

commandNotSupported :: String -> [String] -> IO ()
commandNotSupported command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"

add :: [String] -> IO ()
add [] = putStrLn "No arguments were given for add action"
add [_] = putStrLn "No new tasks were given for add action"
add (fileName:newTasks) = appendFile fileName (unlines newTasks)

view :: [String] -> IO ()
view [] = putStrLn "No file was specified for view action"
view [fileName] = do
  contents <- readFile fileName
  let
    numberedTasks = zipWith (\n s -> show n ++ " - " ++ s) [0..] $ lines contents
  mapM_ putStrLn numberedTasks
view (_:_) = putStrLn "Too many arguments fed to view action"

remove :: [String] -> IO ()
remove [] = putStrLn "No arguments were given for remove action"
remove [_] = putStrLn "No tasks were marked for remove action"
remove (fileName:indexes) = do
  content <- readFile fileName
  let
    contentLines = lines content
    numberedTasks = zipWith (\n s -> show n ++ " - " ++ s) [0..] contentLines
  putStrLn "These are your old TO-DO items:"
  mapM_ putStrLn numberedTasks
  let
    -- Parsed indexes.
    indexesP = foldr (\n acc -> (read n :: Int):acc) [] indexes
    newContentLines = foldr (\t acc -> if fst t `elem` indexesP then acc else snd t : acc) [] (zip [0..] contentLines)
    newNumberedTasks = zipWith (\n s -> show n ++ " - " ++ s) [0..] newContentLines
  putStrLn "These are your new TO-DO items:"
  mapM_ putStrLn newNumberedTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      mapM_ (hPutStrLn tempHandle) newContentLines
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName)
