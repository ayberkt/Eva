module Eval where

eval (List (Atom func : args)) = apply func $ map eval args
