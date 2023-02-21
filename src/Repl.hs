{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import           Control.Monad.State (liftIO)
import           Lens.Micro.TH (makeLenses)
import           System.Console.Haskeline (defaultSettings, getInputLine
                                         , outputStrLn, runInputT, InputT)
import           System.Process (callCommand)
import           Data.Char (isSpace)
import           Eval (betaReduction, runLambda)
import           Parser (parser)

data Flags = Flags { _parse :: Bool
                   , _reduction :: Bool
                   , _replMode :: Bool
                   , _noEval :: Bool
                   }
  deriving Show
makeLenses ''Flags

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "λ> "
      case input of
        Just (':':'b':xs) -> do
          case parser (dropWhile isSpace xs) of
            Left e  -> outputStrLn (show e)
            Right a -> outputStrLn $ betaReduction a
          loop
        Just (':':'p':xs) -> do
          case parser (dropWhile isSpace xs) of
            Left e  -> outputStrLn (show e)
            Right a -> outputStrLn $ show a
          loop
        Just (':':'!':xs) -> do
          liftIO $ callCommand (dropWhile isSpace xs)
          loop
        Just (':':'r':xs) -> do
          file <- liftIO
            $ readFile
              (reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace xs)
          case runLambda file (+ (1 :: Int)) of
            Left e  -> outputStrLn e
            Right a -> outputStrLn $ show (a 0)
          loop
        Just (':':'q':_) -> return ()
        Just (':':'h':_) -> outputStrLn helpTxt >> loop
        Just x -> do
          case runLambda x (+ (1 :: Int)) of
            Left e  -> outputStrLn e
            Right a -> outputStrLn $ show (a 0)
          loop
        Nothing -> return ()

    helpTxt = unlines
      [ "<expr>                          Evaluate an expression"
      , ":b              <expr>          Do a beta reduction"
      , ":h                              Print Help"
      , ":q                              Quit"
      , ":p              <expr>          Print the generated syntax tree"
      , ":r              <file>          Run the provided file"
      , ":!              <cmd>           Run a shell command"]
