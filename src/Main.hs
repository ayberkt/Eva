module Main where

import System.Console.Haskeline (getInputLine, outputStrLn, runInputT
                                , defaultSettings)
import Control.Monad.Trans (liftIO)

import Parser (readExpr)
import Eval (eval)

main :: IO ()
main = do runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "eva> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        -- Just input -> (liftIO $ putStrLn $ Parser.readExpr input) >> loop
        Just input -> (liftIO . print . eval $ readExpr input) >> loop
