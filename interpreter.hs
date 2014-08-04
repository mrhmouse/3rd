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
  "+" -> add stack
  "-" -> sub stack
  "*" -> mul stack
  "/" -> divide stack
  s -> case Map.lookup s e of
    Nothing -> error $ "Undefined name: " ++ s
    Just v -> case v of
      Block b -> runBlock e b stack
      literal -> literal:stack

runBlock :: Env -> [Value] -> [Value] -> [Value]
runBlock e [] stack = stack
runBlock e (v:vs) stack = runBlock e vs (runValue e v stack)

underflow = error "Stack underflow!"

dup [] = underflow
dup (x:xs) = x:x:xs

swap (a:b:xs) = b:a:xs
swap _ = underflow

rot (a:b:c:xs) = b:c:a:xs
rot _ = underflow

dip (a:b:c:xs) = c:a:b:xs
dip _ = underflow

add (a:b:xs) = s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ i + j
    (Float i, Float j) -> Float $ i + j
    (Float i, Int j) -> Float $ i + fromIntegral j
    (Int i, Float j) -> Float $ j + fromIntegral i
    (i, j) -> error $
      "Can only add numeric types, not " ++
      show i ++ " and " ++ show j
add _ = underflow

sub (a:b:xs) = s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ j - i
    (Float i, Float j) -> Float $ j - i
    (Float i, Int j) -> Float $ (fromIntegral j) - i
    (Int i, Float j) -> Float $ (fromIntegral i) - j
    (i, j) -> error $
      "Can only subtract numeric types, not " ++
      show i ++ " and " ++ show j
sub _ = underflow

mul (a:b:xs) = s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ j * i
    (Float i, Float j) -> Float $ j * i
    (Float i, Int j) -> Float $ (fromIntegral j) * i
    (Int i, Float j) -> Float $ (fromIntegral i) * j
    (i, j) -> error $
      "Can only multiply numeric types, not " ++
      show i ++ " and " ++ show j
mul _ = underflow

divide (a:b:xs) = s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Float $ (fromIntegral j) / (fromIntegral i)
    (Float i, Float j) -> Float $ j / i
    (Float i, Int j) -> Float $ (fromIntegral j) / i
    (Int i, Float j) -> Float $ (fromIntegral i) / j
    (i, j) -> error $
      "Can only divide numeric types, not " ++
      show i ++ " and " ++ show j
divide _ = underflow
