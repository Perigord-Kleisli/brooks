{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import qualified Control.Monad.State           as S
import           Control.Monad.State            ( liftIO )
import           Lens.Micro
import           Lens.Micro.TH
import           System.Console.Readline
import           System.Process

import           Control.Exception
import           Control.Monad
import           Data.Char
import           System.IO

import           Eval
import           Parser


data Flags = Flags
  { _parse     :: Bool
  , _reduction :: Bool
  , _replMode  :: Bool
  , _noEval    :: Bool
  }
  deriving Show

makeLenses ''Flags

data ReplEnv = ReplEnv
  { _opts     :: Flags
  , _lastCmd  :: String
  , _lastFile :: FilePath
  }
  deriving Show
makeLenses ''ReplEnv

repl :: Flags -> IO ()
repl f = (`S.evalStateT` ReplEnv f "" "") runRepl

runRepl :: S.StateT ReplEnv IO ()
runRepl = do
  liftIO $ setFilenameCompletionDesired True
  maybeLine <- liftIO $ readline "λ> "

  case maybeLine of
    Nothing   -> liftIO $ hPutStrLn stderr "EOF Reached"
    Just line -> do
      liftIO $ addHistory line
      settings'  <- S.gets _opts
      commands settings' line
 where
  evalFile settings file =
    try (readFile $ reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace file)
      >>= either (hPrint @IOError stderr) (outputFunc settings)

  outputFunc settings input = liftIO $ do
    when (settings ^. parse)
      $   either (hPrint stderr) (putStrLn . showTree)
      $   unappendAdd
      <$> parser input

    when (settings ^. reduction)
      $   either (hPrint stderr) (putStrLn . betaReduction)
      $   unappendAdd
      <$> parser input

    unless (settings ^. noEval)
      $ either (hPutStrLn stderr) (print @Int . ($ 0))
      $ (`runLambda` (+ 1)) input

  commands settings =
      \case
        cmd@(':' : 'b' : x) -> do
          outputFunc (settings & reduction .~ True & noEval .~ True) $ dropWhile isSpace x
          S.modify (lastCmd .~ cmd)
          runRepl

        cmd@(':' : 'h' : _) -> do
          liftIO $ putStrLn helpTxt
          S.modify (lastCmd .~ cmd)
          runRepl

        cmd@(':' : 'p' : x) -> do
          outputFunc (settings & parse .~ True & noEval .~ True) $ dropWhile isSpace x
          S.modify (lastCmd .~ cmd)
          runRepl

        ':' : 'q' : _ -> do
          liftIO $ putStrLn "Leaving BROOKS."

        cmd@(':' : 'r' : 'b' : x) -> do
          lastFile' <- S.gets _lastFile
          let file = if null (dropWhile isSpace x) 
                        then lastFile'
                        else x
          liftIO $ evalFile (settings & reduction .~ True & noEval .~ True) file
          S.modify (lastCmd .~ cmd)
          S.modify (lastFile .~ file)
          runRepl

        cmd@(':' : 'r' : 'p' : x) -> do
          lastFile' <- S.gets _lastFile
          let file = if null (dropWhile isSpace x)
                        then lastFile'
                        else x
          liftIO $ evalFile (settings & parse .~ True & noEval .~ True) file
          S.modify (lastCmd .~ cmd)
          S.modify (lastFile .~ file)
          runRepl

        cmd@(':' : 'r' : x) -> do
          lastFile' <- S.gets _lastFile
          let file = if null (dropWhile isSpace x) 
                        then lastFile'
                        else x
          liftIO $ evalFile settings file
          S.modify (lastCmd .~ cmd)
          S.modify (lastFile .~ file)
          runRepl

        cmd@(':' : '!' : x) -> do
          liftIO $ try (callCommand x) >>= either (print @IOException) (const mempty)
          S.modify (lastCmd .~ cmd)
          runRepl

        [':'] -> do
          S.gets _lastCmd >>= commands settings

        x -> do
          outputFunc settings x
          S.modify (lastCmd .~ x)
          runRepl

  helpTxt = unlines
    [ "<expr>                          Evaluate an expression"
    , ":                               Repeat last command"
    , ":b              <expr>          Do a beta reduction"
    , ":h                              Print Help"
    , ":q              <expr>          Quit"
    , ":p              <expr>          Print the generated syntax tree"
    , ":r              <file>          Run the provided file"
    , ":!              <cmd>           Run a shell command"
    ]




