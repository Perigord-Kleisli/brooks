module Parser where 

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Arrow

lexeme :: Parser a -> Parser a
lexeme = (<* (spaces <|> void newline))

symbol :: Char -> Parser ()
symbol = void . lexeme . char

data Lambda = Identifier String
            | Application Lambda Lambda
            | Abstraction String Lambda
            | Grouping    Lambda
            | Number Int
            | Lambda :+: Lambda
  deriving Show

identifier :: Parser String
identifier = lexeme $ many1 (noneOf "λ.()\\\n1234567890 ")

num :: Parser Int
num = lexeme $ read <$> many1 digit

-- 0 : \f a . a
-- 1 : \f a . f a 
-- 2 : \f a . f (f a)

churchNum :: Parser Lambda
churchNum = applyN <$> num
  where 
    applyN n = Grouping $ Abstraction "f" $ Abstraction "n" (applyN' "f" "n" n)
    applyN' func var 0 = Grouping (Identifier var)
    applyN' func var 1 = Grouping (Application (Identifier func) (Identifier var))
    applyN' func var n = Grouping (Application (Identifier func) (applyN' func var (n-1)))

variable :: Parser Lambda
variable = Identifier <$> identifier

application :: Parser Lambda
application = liftA2 Application lambdaP lambdaP

abstraction :: Parser Lambda
abstraction = liftA2 (flip $ foldr Abstraction)
                          ((symbol 'λ' <|> symbol '\\') *> many1 identifier <* symbol '.')
                          lambdaP

grouping :: Parser Lambda
grouping = Grouping <$> (symbol '(' `between` symbol ')') lambdaP

lterm :: Parser Lambda
lterm = choice [try variable, churchNum, abstraction,grouping]

lambdaP :: Parser Lambda
lambdaP = chainl1 lterm (Application <$ spaces) 

parser :: String -> Either ParseError Lambda
parser = fmap appendAdd <$> parse lambdaP "Lambda Calculus: "
  where
    appendAdd x = Application
                    (Application x 
                      (Abstraction ".topX" 
                          (Identifier ".topX" :+: Number 1)))
                    (Number 0)

