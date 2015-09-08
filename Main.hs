module Main where

import Text.ParserCombinators.Parsec ( Parser, string, many, char, alphaNum
                                     , letter, (<|>),  digit, space, many1
                                     , oneOf, skipMany1, parse, sepBy, try
                                     , endBy)
import Text.ParserCombinators.Parsec.Token (identifier)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (getInputLine, outputStrLn, runInputT
                                , defaultSettings)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom s) = "Atom " ++ s
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (DottedList xs x) = "Dotted list " ++ show xs ++ " . " ++ show x
  show (Number x) = show x
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b) = show b

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedQuote :: Parser Char
escapedQuote = char '\\' >> char '"'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (alphaNum <|> space <|> symbol <|> escapedQuote)
  char '"'
  return $ String x

{- An attom is a letter or symbol, followed by any number of letters,-}
{- digits, or symbols. -}
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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail
            
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> (char '(' >> (try parseList <|> parseDottedList)
                       >>= \x -> char ')'
                       >> return x)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right x  -> show x

main :: IO ()
main = do runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "eva> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ putStrLn $ readExpr input) >> loop
