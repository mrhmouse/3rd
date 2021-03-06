module Interpreter where
import qualified Data.Map.Lazy as Map
import Parser
import Control.Monad

type Env = Map.Map Name Value
type State = (Env, [Value])

interpret :: Env -> [Statement] -> IO State
interpret e [] = return (e, [])
interpret e stmts = i e stmts [] where
  i e [] stack = return (e, reverse stack)
  i e (s:rest) stack = case s of
    Definition n v -> i (Map.insert n v e) rest stack
    Value v        -> do result <- runValue e v stack
                         i e rest result
    Import n       -> do source <- readFile n
                         e' <- evalProgram e source
                         i e' rest stack

evalProgram :: Env -> String -> IO Env
evalProgram e s = do
  let stmts = readProgram s
  (e', _) <- interpret e stmts
  return e'

execProgram :: String -> IO [Value]
execProgram s = do
  let e = Map.fromList []
  let stmts = readProgram s
  (e', end) <- interpret e stmts
  return end

runValue :: Env -> Value -> [Value] -> IO [Value]
runValue e v stack = case v of
  Name n  -> runName e n stack
  literal -> return $ literal:stack

-- This is where builtin functions live.
runName :: Env -> Name -> [Value] -> IO [Value]
runName e n stack = case n of
  "eval" -> eval e stack
  "yank" -> yank stack
  "copy" -> copy stack
  "delete" -> delete stack
  "string" -> toString stack
  "+" -> add stack
  "-" -> sub stack
  "*" -> mul stack
  "/" -> divide stack
  "if" -> conditional e stack
  "while" -> while e stack
  "=" -> equals stack
  ">" -> greaterThan stack
  "<" -> lessThan stack
  "!" -> inverse stack
  "." -> printVal stack
  "call" -> runBlock e block rest where
    (Block block) = head stack
    rest = drop 1 stack
  "pop" -> pop stack
  "push" -> push stack
  "cat" -> cat stack
  "empty" -> empty stack
  "length" -> valLength stack
  "pluck" -> pluck stack
  "insert" -> insert stack
  s -> case Map.lookup s e of
    Nothing -> error $ "Undefined name: " ++ s
    Just v -> case v of
      Block b -> runBlock e b stack
      literal -> return $ literal:stack

while e (Block cond:Block body:rest) = do
  (Bool b:rest) <- runBlock e cond rest
  if b
  then do rest <- runBlock e body rest
          while e (Block cond:Block body:rest)
  else return rest

runBlock :: Env -> [Value] -> [Value] -> IO [Value]
runBlock e [] stack = return stack
runBlock e (v:vs) stack = do
  result <- runValue e v stack
  runBlock e vs result

underflow = error "Stack underflow!"

copy [] = underflow
copy (Int n':stack) = if n < len then return $ yanked:stack else underflow where
  n = fromIntegral n'
  len = length stack
  yanked = stack !! n
copy (a:_) = error $ "Expected integer, but got: " ++ show a

yank [] = underflow
yank (Int n':stack) = if n < len then return $ yanked:rest else underflow where
  n = fromIntegral n'
  len = length stack
  yanked = stack !! n
  rest = stack `except` n
yank (a:_) = error $ "Expected integer, but got: " ++ show a

delete [] = underflow
delete (Int n':stack) = if n < len then return $ rest else underflow where
  n = fromIntegral n'
  len = length stack
  rest = stack `except` n
delete (a:_) = error $ "Expected integer, but got: " ++ show a

toString [] = underflow
toString (a:rest) = return $ str:rest where
  str = String $ printValue a

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

greaterThan (a:b:stack) = return $ result:stack where result = Bool $ b > a
greaterThan _ = underflow

lessThan (a:b:stack) = return $ result:stack where result = Bool $ b < a
lessThan _ = underflow

inverse (Bool a:stack) = return $ result:stack where result = Bool $ not a
inverse (a:stack) = error $ "Expected a boolean, but got " ++ show a
inverse _ = underflow

printVal (v:rest) = do
  putStr $ printValue v
  return rest

joinSpaces :: [String] -> String
joinSpaces strs = foldl f "" strs where f a b = a ++ " " ++ b

printValue :: Value -> String
printValue v = case v of
  Bool v -> show v
  Int v -> show v
  String v -> v
  Float v -> show v
  List v -> "(" ++ joinSpaces (map printValue v) ++ ")"
  Block v -> "[" ++ joinSpaces (map printValue v) ++ "]"
  Name v -> v

pop (lst:rest) = return $ h:t:rest where
  (construct, items) = case lst of
    List items -> (List, items)
    String items -> (String . join . map printValue, map (String . show) items)
    Block items -> (Block, items)
    _ -> error $ "Expected list, but got " ++ show lst
  h = head items
  t = construct $ tail items
pop _ = underflow

cat (List a:List b:rest) = return $ (List $ a++b):rest
cat (Block a:Block b:rest) = return $ (Block $ a++b):rest
cat (String a:String b:rest) = return $ (String $ a++b):rest
cat (a:b:rest) = error $ "Expected two lists or two strings, but got " ++ show a ++ " and " ++ show b
cat _ = underflow

push (a:List b:rest) = return $ (List $ a:b):rest
push (a:Block b:rest) = return $ (Block $ a:b):rest
push (a:String b:rest) = return $ (String $ printValue a ++ b):rest
push (_:b:rest) = error $ "Expected a list, but got " ++ show b
push _ = underflow

empty (List a:rest) = return $ Bool (null a):rest
empty (String a:rest) = return $ Bool (null a):rest
empty (Block a:rest) = return $ Bool (null a):rest
empty (a:rest) = error $ "Expected a list, but got " ++ show a
empty _ = underflow

valLength (List a:rest) = return $ Int (fromIntegral $ length a):rest
valLength (String a:rest) = return $ Int (fromIntegral $ length a):rest
valLength (Block a:rest) = return $ Int (fromIntegral $ length a):rest
valLength (a:rest) = error $ "Expected a list, but got " ++ show a
valLength _ = underflow

except :: [a] -> Int -> [a]
except lst n = h ++ t where
  h = take n lst
  t = drop (n + 1) lst

pluck (Int n':lst:rest) = return $ nth:lst':rest where
  n = fromIntegral n'
  items = case lst of
    List items -> items
    Block items -> items
    String chars -> map (String . show) chars
  nth = case items !! n of
    Name n -> Block [Name n]
    lit -> lit
  lst' = case lst of
    List xs -> List $ xs `except` n
    Block xs -> Block $ xs `except` n
    String xs -> String $ xs `except` n
pluck (a:_:_) = error $ "Execpted integer, but got: " ++ show a
pluck _ = underflow

insert (Int n':a:String str:rest) = return $ lst':rest where
  n = fromIntegral n'
  (h, t) = n `splitAt` str
  lst' = String $ h ++ printValue a ++ t
insert (Int n':a:List lst:rest) = return $ lst':rest where
  n = fromIntegral n'
  (h, t) = n `splitAt` lst
  lst' = List $ h ++ [a] ++ t
insert (Int n':a:Block lst:rest) = return $ lst':rest where
  n = fromIntegral n'
  (h, t) = n `splitAt` lst
  lst' = Block $ h ++ [a] ++ t
insert (Int _:_:a:_) = error $ "Expected list type, but got: " ++ show a
insert (a:_:_:_) = error $ "Expected integer, but got: " ++ show a
insert _ = underflow

eval e (String str:rest) = do
  let parsed = readProgram str
  (_, stack) <- interpret e parsed
  return stack

eval _ (a:_) = error $ "Expected a string, but got: " ++ show a
eval _ _ = underflow
