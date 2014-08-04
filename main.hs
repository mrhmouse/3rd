module Main where
import Interpreter
import Parser
import qualified Data.Map.Lazy as Map

execProgram :: String -> [Value]
execProgram s = end where
  (e', end) = interpret e stmts
  e = Map.fromList []
  stmts = readProgram s

main = do
  s <- getContents
  print $ execProgram s
