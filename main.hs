module Main where
import Interpreter
import Parser
import qualified Data.Map.Lazy as Map
import Control.Monad

main = do
  s <- getContents
  result <- execProgram s
  if not (null result)
  then do putStr "\nLeft on the stack: "
          print result
  else putStr ""
