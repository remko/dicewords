import Dicewords
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit

{-# ANN options "HLint: ignore Use string literal" #-}
options :: [OptDescr ()]
options = []

main :: IO ()
main = do 
  args <- getArgs
  case getOpt Permute options args of
    (_, inputs, []) -> do
      (listFile:scoreFiles) <- mapM readFile inputs
      let result = unlines $ map formatScore $ scoreList listFile scoreFiles
      putStr result

    (_, _, errs) -> do
      putStrLn $ concat errs ++ usageInfo header options
      exitWith $ ExitFailure 1
  where
    header = "Usage: dicewords-create-candidate-list input-files"

    formatScore :: (String, Float) -> String
    formatScore (x, y) = x ++ " " ++ show y
