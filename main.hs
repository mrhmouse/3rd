module Main where
import Interpreter
import Parser
import qualified Data.Map.Lazy as Map
import Control.Monad

execProgram :: String -> IO [Value]
execProgram s = do
  let e = Map.fromList []
  let stmts = readProgram s
  (e', end) <- interpret e stmts
  return end

main = do
  s <- getContents
  result <- execProgram s
  if not (null result)
  then do putStr "\nLeft on the stack: "
          print result
  else putStr ""
