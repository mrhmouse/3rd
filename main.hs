module Main where
import Interpreter
import Parser
import qualified Data.Map.Lazy as Map

execProgram :: String -> IO [Value]
execProgram s = do
  let e = Map.fromList []
  let stmts = readProgram s
  (e', end) <- interpret e stmts
  return end

main = do
  s <- getContents
  result <- execProgram s
  putStr "\n"
  putStr "Left on the stack: "
  print result
