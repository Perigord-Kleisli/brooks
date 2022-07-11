module VM where

import Parser
import qualified  Data.Map.Strict as M
import qualified  Control.Monad.State as T

type TopLvlFunc a = (a -> a) -> a -> a
type Error = [String]

newtype Interpreter = Interpreter {
    symbolTable :: M.Map String Lambda
  }
data VM = VM {
    codePointer :: Int
  , stack :: [Int]
  }

runLambda :: String -> (a -> a) -> a -> a
runLambda = either error id 
          . undefined
          . either (error . show) id . parser

runLambda' str = runLambda str (+1) 0
