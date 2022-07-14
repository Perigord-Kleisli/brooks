{-# LANGUAGE LambdaCase #-}
module Parser where

import qualified Control.Monad.State as S
import qualified Data.Map.Lazy as M
import Text.ParserCombinators.Parsec

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Function
import Text.Printf

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
    applyN' _    var 0 = Grouping (Identifier var)
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
parser = fmap appendAdd <$> parse lambdaP "Brooks: "
  where
    appendAdd x = Application
                    (Application x
                      (Abstraction ".topX"
                          (Identifier ".topX" :+: Number 1)))
                    (Number 0)


alphaConvert :: Lambda -> Lambda
alphaConvert = (`S.evalState` mempty) . go
  where
    go :: Lambda -> S.State (M.Map String Int) Lambda
    go (Abstraction arg expr) = do
      S.modify (M.insertWith(+) arg 1)
      arg' <- S.gets (mappend arg . (\case; x | x <= 1 -> ""; x -> '.': show x) . M.findWithDefault 0 arg)
      expr' <- go expr
      return (Abstraction arg' expr')

    go (Identifier arg) = do
      arg' <- S.gets (mappend arg . (\case; x | x <= 1 -> ""; x -> '.': show x) . M.findWithDefault 0 arg)
      return $ Identifier arg'

    go (Application x y) = (liftA2 (flip Application) `on` go) y x

    go (Grouping x) = Grouping <$> go x
    go x = return x

unappendAdd :: Lambda -> Lambda
unappendAdd (Application (Application x (Abstraction ".topX" (Identifier ".topX" :+: Number 1))) (Number 0)) = x
unappendAdd x = x

showLambda :: Lambda -> String
showLambda (Identifier x     ) = takeWhile (/='.') x
showLambda (Number     x     ) = show x
showLambda (x           :+: y) = (printf "(+ %s %s)" `on` showLambda) x y
showLambda (Application x   y) = (printf "%s %s" `on` showLambda) x y
showLambda (Abstraction x   y) = (printf "λ%s.%s" `on` showLambda) (Identifier x) y
showLambda (Grouping x       ) = printf "(%s)" $ showLambda x

showTree :: Lambda -> String
showTree = go 0
 where
  indent x y = mappend "\n" (concat $ replicate x "  ") <> y
  go :: Int -> Lambda -> String
  go n (Number x       ) = indent n $ printf "(Number %s)" $ show x
  go n (x :+: y        ) = indent n $ (printf "(:+: %s %s)" `on` go (n + 1)) x y
  go n (Identifier x   ) = indent n $ printf "(Identifier %s)" x
  go n (Grouping   x   ) = indent n $ printf "(Grouping %s)" (go (n + 1) x)
  go n (Application x y) = indent n $ (printf "(Application %s %s)" `on` go (n + 1)) x y
  go n (Abstraction x y) =
    indent n $ (printf "(Abstraction %s %s)" `on` go (n + 1)) (Identifier x) y
