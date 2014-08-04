module Interpreter where
import qualified Data.Map.Lazy as Map
import Parser

type Env = Map.Map Name Value
type State = (Env, [Value])

interpret :: Env -> [Statement] -> State
interpret e [] = (e, [])
interpret e stmts = i e stmts [] where
  i e [] stack = (e, stack)
  i e (s:rest) stack = case s of
    Definition n v -> i (Map.insert n v e) rest stack
    Value v        -> i e rest (runValue e v stack)

runValue :: Env -> Value -> [Value] -> [Value]
runValue e v stack = case v of
  Name n  -> runName e n stack
  literal -> literal:stack

runName :: Env -> Name -> [Value] -> [Value]
runName e n stack = case n of
  "dup" -> dup stack
  "drop" -> drop 1 stack
  "swap" -> swap stack
  "rot" -> rot stack
  "dip" -> dip stack
  s -> error $ "User-defined functions are unimplemented! " ++ s

underflow = error "Stack underflow!"

dup [] = underflow
dup (x:xs) = x:x:xs

swap (a:b:xs) = b:a:xs
swap _ = underflow

rot (a:b:c:xs) = b:c:a:xs
rot _ = underflow

dip (a:b:c:xs) = c:a:b:xs
dip _ = underflow
