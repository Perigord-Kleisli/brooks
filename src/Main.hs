{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Monad.Except
import           Lens.Micro

import           Control.Arrow
import           System.Console.GetOpt
import           System.Environment
import           System.IO

import           Eval
import           Parser
import           Repl

defaultFlags :: Flags
defaultFlags = Flags False False False False

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option [] ["parse"]     (NoArg (parse .~ True))     "Show the syntax tree"
  , Option [] ["reduction"] (NoArg (reduction .~ True)) "Do a β reduction"
  , Option [] ["repl"]      (NoArg (replMode .~ True))      "Enable the REPL"
  ]

compileOpts :: [String] -> IO (Flags, [String])
compileOpts argv = case getOpt RequireOrder options argv of
                     (flags, args, []  ) -> return (foldl (flip id) defaultFlags flags, args)
                     (_    , _   , errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: brooks [OPTION...] files..."



main :: IO ()
main = do
  (settings, files) <-
    getArgs >>= fmap (second $ mapM readFile) . compileOpts >>= sequence
  if settings ^. replMode
    then repl settings
    else do
      when (settings ^. parse)
        $ forM_ files (either (hPrint stderr) (putStrLn . showTree . unappendAdd) . parser)
      when (settings ^. reduction)
        $ forM_ files (either (hPrint stderr) (putStrLn . betaReduction . unappendAdd) . parser)

      forM_ files (either (hPutStrLn stderr) (print @Int . ($ 1)) . (`runLambda` (+ 1)))
