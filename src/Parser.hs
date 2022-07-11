module Parser where 

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Arrow

data InterpreterError = Parser ParseError | Compiler String

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

symbol :: Char -> Parser ()
symbol = void . lexeme . char

data Lambda = Identifier String
            | Application Lambda Lambda
            | Abstraction Lambda Lambda
            | Grouping    Lambda
  deriving Show

identifier :: Parser String
identifier = lexeme $ many1 (noneOf "λ.()\\ ")

variable :: Parser Lambda
variable = Identifier <$> identifier

application :: Parser Lambda
application = liftA2 Application lambdaP lambdaP

abstraction :: Parser Lambda
abstraction = liftA2 (flip $ foldr Abstraction)
                          ((symbol 'λ' <|> symbol '\\') *> many1 (Identifier <$> identifier) <* symbol '.')
                          lambdaP

grouping :: Parser Lambda
grouping = Grouping <$> (symbol '(' `between` symbol ')') lambdaP

lterm :: Parser Lambda
lterm = choice [try variable,abstraction,grouping]

lambdaP :: Parser Lambda
lambdaP = chainl1 lterm (Application <$ spaces) 

parser :: String -> Either InterpreterError Lambda
parser = left Parser . parse lambdaP "Lambda Calculus: "

