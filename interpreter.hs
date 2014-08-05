module Interpreter where
import qualified Data.Map.Lazy as Map
import Parser

type Env = Map.Map Name Value
type State = (Env, [Value])

interpret :: Env -> [Statement] -> IO State
interpret e [] = return (e, [])
interpret e stmts = i e stmts [] where
  i e [] stack = return (e, reverse stack)
  i e (s:rest) stack = case s of
    Definition n v -> i (Map.insert n v e) rest stack
    Value v        -> do
      result <- runValue e v stack
      i e rest result

runValue :: Env -> Value -> [Value] -> IO [Value]
runValue e v stack = case v of
  Name n  -> runName e n stack
  literal -> return $ literal:stack

-- This is where builtin functions live.
runName :: Env -> Name -> [Value] -> IO [Value]
runName e n stack = case n of
  "dup" -> dup stack
  "drop" -> return $ drop 1 stack
  "swap" -> swap stack
  "rot" -> rot stack
  "dip" -> dip stack
  "+" -> add stack
  "-" -> sub stack
  "*" -> mul stack
  "/" -> divide stack
  "if" -> conditional e stack
  "=" -> equals stack
  ">" -> greaterThan stack
  "<" -> lessThan stack
  "!" -> inverse stack
  "call" -> runBlock e block rest where
    (Block block) = head stack
    rest = drop 1 stack
  s -> case Map.lookup s e of
    Nothing -> error $ "Undefined name: " ++ s
    Just v -> case v of
      Block b -> runBlock e b stack
      literal -> return $ literal:stack

runBlock :: Env -> [Value] -> [Value] -> IO [Value]
runBlock e [] stack = return stack
runBlock e (v:vs) stack = do
  result <- runValue e v stack
  runBlock e vs result

underflow = error "Stack underflow!"

dup [] = underflow
dup (x:xs) = return $ x:x:xs

swap (a:b:xs) = return $ b:a:xs
swap _ = underflow

rot (a:b:c:xs) = return $ b:c:a:xs
rot _ = underflow

dip (a:b:c:xs) = return $ c:a:b:xs
dip _ = underflow

add (a:b:xs) = return $ s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ i + j
    (Float i, Float j) -> Float $ i + j
    (Float i, Int j) -> Float $ i + fromIntegral j
    (Int i, Float j) -> Float $ j + fromIntegral i
    (i, j) -> error $
      "Can only add numeric types, not " ++
      show i ++ " and " ++ show j
add _ = underflow

sub (a:b:xs) = return $ s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ j - i
    (Float i, Float j) -> Float $ j - i
    (Float i, Int j) -> Float $ (fromIntegral j) - i
    (Int i, Float j) -> Float $ (fromIntegral i) - j
    (i, j) -> error $
      "Can only subtract numeric types, not " ++
      show i ++ " and " ++ show j
sub _ = underflow

mul (a:b:xs) = return $ s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Int $ j * i
    (Float i, Float j) -> Float $ j * i
    (Float i, Int j) -> Float $ (fromIntegral j) * i
    (Int i, Float j) -> Float $ (fromIntegral i) * j
    (i, j) -> error $
      "Can only multiply numeric types, not " ++
      show i ++ " and " ++ show j
mul _ = underflow

divide (a:b:xs) = return $ s:xs where
  s = case (a, b) of
    (Int i, Int j) -> Float $ (fromIntegral j) / (fromIntegral i)
    (Float i, Float j) -> Float $ j / i
    (Float i, Int j) -> Float $ (fromIntegral j) / i
    (Int i, Float j) -> Float $ (fromIntegral i) / j
    (i, j) -> error $
      "Can only divide numeric types, not " ++
      show i ++ " and " ++ show j
divide _ = underflow

conditional e (Block b:Block false:Block true:stack) = do
  result <- runBlock e b stack
  let Bool b = head result
  let rest = drop 1 result
  let choice = if b then true else false
  runBlock e choice rest

conditional _ _ = underflow

equals (a:b:stack) = return $ result:stack where result = Bool $ a == b
equals _ = underflow

greaterThan (a:b:stack) = return $ result:stack where result = Bool $ a > b
greaterThan _ = underflow

lessThan (a:b:stack) = return $ result:stack where result = Bool $ a < b
lessThan _ = underflow

inverse (Bool a:stack) = return $ result:stack where result = Bool $ not a
inverse (a:stack) = error $ "Expected a boolean, but got " ++ show a
inverse _ = underflow
