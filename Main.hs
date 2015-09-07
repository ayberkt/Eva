module Main where

import Text.ParserCombinators.Parsec ( Parser, string, many, char, alphaNum
                                     , letter, (<|>),  digit, space, many1
                                     , oneOf, skipMany1, parse)
import Text.ParserCombinators.Parsec.Token (identifier)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (getInputLine, outputStrLn)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many alphaNum <|> string "\\\""
  return $ String x

{- An attom is a letter or symbol, followed by any number of letters,-}
{- digits, or symbols.
-}
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
