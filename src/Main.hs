import Dicewords
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO (openFile, hSetEncoding, hGetContents, IOMode(ReadMode), utf8)

data Flag = Verbose | Output String | OutputMode Mode deriving (Show, Eq)
   
options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "print statistics"
  , Option ['o'] ["output"] (ReqArg Output "FILE") "output FILE"
  , Option [] ["output-mode"] (ReqArg (OutputMode . parseOutputMode) "MODE")
      "Output mode. One of diceware (default), diceware8k, all"
  ]

parseOutputMode :: String -> Mode
parseOutputMode "diceware8k" = Diceware8kMode
parseOutputMode "diceware" = DicewareMode
parseOutputMode "all" = AllWordsMode
parseOutputMode mode = error $ "Invalid output mode: " ++ mode

main :: IO ()
main = do 
  args <- getArgs
  case getOpt Permute options args of
    (opts, [input], []) -> do
      let outputFile = case [x | Output x <- opts] of
                        [] -> "dicewords.txt"
                        (x:_) -> x
      let outputMode = case [x | OutputMode x <- opts] of
                        [] -> DicewareMode
                        (x:_) -> x

      content <- readFile' input
      let (output, statistics) = process content outputMode
      writeFile outputFile output
      if elem Verbose opts
        then putStrLn $ show statistics
        else return ()

    (_, _, errs) -> do
      putStrLn $ concat errs ++ usageInfo header options
      exitWith $ ExitFailure 1
  where
    header = "Usage: dicewords [OPTION...] input-file"

    readFile' input = do
      h <- openFile input ReadMode
      hSetEncoding h utf8
      hGetContents h
