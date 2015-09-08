module Main where

import System.Console.Haskeline (getInputLine, outputStrLn, runInputT
                                , defaultSettings)
import Control.Monad.Trans (liftIO)

import qualified Parser

main :: IO ()
main = do runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "eva> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ putStrLn $ Parser.readExpr input) >> loop
