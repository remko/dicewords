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
      files <- mapM readFile inputs
      let result = unlines $ createCandidateList files
      putStr result

    (_, _, errs) -> do
      putStrLn $ concat errs ++ usageInfo header options
      exitWith $ ExitFailure 1
  where
    header = "Usage: dicewords [OPTION...] input-file score-file..."
