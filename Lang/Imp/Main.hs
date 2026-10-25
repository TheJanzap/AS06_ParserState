module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  (fileName, state) <- parseArgs
  content <- readFile fileName
  putStrLn "Input State:"
  printState state
  case executeProgram content state of
    Left err -> putStrLn err
    Right finalState -> do
      putStrLn "Result State:"
      printState finalState
  
-- Parses the program's arguments and returns the file name and the initial state
parseArgs :: IO (String, State)
parseArgs = do
  args <- getArgs
  case args of
    [] -> error "Expected a file name"
    (filename : vars) -> case mapM parseVar vars of
      Just vs -> return (filename, M.fromList vs)
      Nothing -> error "Expected variables in key=value format"

-- Parses a key-value variable pair in the form of `variable=1`
parseVar :: String -> Maybe (Name, Integer)
-- Since Haskell doesn't seem to have a split string function, we have to misuse `break`
parseVar arg = case break (== '=') arg of
  -- break splits the string, but includes the symbol in the second string
  -- So we have to pattern match it away
  (k, '=' : v) -> Just (k, read v)
  _ -> Nothing

-- Prints the given state
printState :: State -> IO ()
printState st =
  mapM_ printVar (M.toAscList st)
  where printVar (name, value) = putStrLn ("\"" ++ name ++ "\" -> " ++ show value)

-- Execute the code in String with the given State.
executeProgram :: String -> State -> Either String State
executeProgram content state = do
  cmd <- parseImp content
  (finalState, _) <- runState (exec cmd) state
  pure finalState