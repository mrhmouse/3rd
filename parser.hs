module Parser where

type Name = String
type ParseStep = String -> (String, Value)

data Value
  = Name Name
  | Int Integer
  | Float Float
  | String String
  | Bool Bool
  | Block [Value]
  | List [Value]
  deriving (Show, Eq, Ord)

data Statement
  = Value Value
  | Definition Name Value
  | Import String
  deriving Show

whitespace :: Char -> Bool
whitespace c = c `elem` " \t\n\r"

alpha :: Char -> Bool
alpha c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

symbol :: Char -> Bool
symbol c = c `elem` "?~!@$%^&*_+-=<>,.{}\\/`'"

digit :: Char -> Bool
digit c = c `elem` ['0'..'9']

readProgram :: String -> [Statement]
readProgram s = go (s, []) where
  go ([], vs) = reverse vs
  go (':':':':s, vs) = let (r, v) = readImport s in go (r, v:vs)
  go (':':s, vs) = let (r, v) = readDefinition s in go (r, v:vs)
  go (s, vs) = case readValue s of
    (r, Nothing) -> go (r, vs)
    (r, Just v)  -> go (r, Value v:vs)

readImport :: String -> (String, Statement)
readImport s = (rest, Import name) where
  (rest', String name) = case dropWhile whitespace s of
    '"':s' -> readString s'
    c:_    -> error $ "Expected a quote,, but got " ++ show c
  rest = case dropWhile whitespace rest' of
    ';':r -> r
    _     -> error "Unterminated import! Expected ';'"

readDefinition :: String -> (String, Statement)
readDefinition s = go (s', []) where
  (s', Name n) = readName (dropWhile whitespace s)
  done = Definition n . Block . reverse
  go ([], vs) = error "Incomplete definition! Expected ';'"
  go (';':s, vs) = (s, done vs)
  go (s, vs) = case readValue s of
    (r, Nothing) -> go (r, vs)
    (r, Just v)  -> go (r, v:vs)

readValue :: String -> (String, Maybe Value)
readValue s = go s where
  done (s, v) = (s, Just v)
  go [] = ([], Nothing)
  go s@(c:r)
    | whitespace c        = go r
    | digit c             = done $ readNumber s
    | alpha c || symbol c = done $ readName s
    | otherwise           = case c of
      '#' -> go $ dropWhile (/= '\n') r
      '[' -> done $ readBlock r
      '(' -> done $ Parser.readList r
      '"' -> done $ readString r
      _   -> (s, Nothing)

readName :: ParseStep
readName s = go (s, []) where
  done s = case reverse s of
    "true"  -> Bool True
    "false" -> Bool False
    n       -> Name n
  go ([], n) = ([], done n)
  go (c:s, n)
    | alpha c || symbol c || digit c = go (s, c:n)
    | otherwise           = (c:s, done n)

readNumber :: ParseStep
readNumber s = go (s, []) where
  done s = let n = reverse s in
    if '.' `elem` n
    then Float $ read n
    else Int $ read n
  go ([], s) = ([], done s)
  go (c:s, n)
    | digit c || c == '.' = go (s, c:n)
    | otherwise           = (c:s, done n)

readBlock :: ParseStep
readBlock s = go (s, []) where
  done = Block . reverse
  go ([], vs) = error "Incomplete block! Expected ']'"
  go (c:s, vs) =
    if c == ']'
    then (s, done vs)
    else case readValue (c:s) of
      (r, Nothing) -> go (r, vs)
      (r, Just v)  -> go (r, v:vs)

readList :: ParseStep
readList s = go (s, []) where
  done = List . reverse
  go ([], vs) = error "Incomplete list! Expected ')'"
  go (c:s, vs) =
    if c == ')'
    then (s, done vs)
    else case readValue (c:s) of
      (r, Nothing) -> go (r, vs)
      (r, Just v)  -> go (r, v:vs)

readString :: ParseStep
readString s = go (s, []) where
  done = String . reverse
  go ([], vs) = error "Incomplete string! Expected '\"'"
  go ('"':s, vs) = (s, done vs)
  go ('\\':'"':s, vs) = go (s, "\"" ++ vs)
  go ('\\':'n':s, vs) = go (s, "\n" ++ vs)
  go ('\\':'t':s, vs) = go (s, "\t" ++ vs)
  go (c:s, vs) = go (s, c:vs)
