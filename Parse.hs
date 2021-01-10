{-
  Provide functions
    parseExp :: String -> Maybe Exp
    parseCom :: String -> Maybe Com


  to parse expressions and commands.
  You can use whatever parser technology you wish, however, we recommend using
  the simple parser combinators from the lecture (which is distributed in PComb.hs with this assignment),
  or the more advanced parsec framework.

  See Readme.md for the syntax of the language.
-}

{- HLINT ignore "Use <$>" -}

module Parse (parseExp,parseCom) where
import While

{- Add your import declarations here -}
import PComb
import GHC.Base
import Pretty
import Data.List

parseVariable = (do
    x <- ident
    return (Var x)
  ) <|> do
    space
    char '-'
    x <- liftM2 (:) ident_start (PComb.many ident_ctd)
    return (Uminus (Var x))

parseConstant = neg <|> pos <|> parseVariable
  where
    pos = do 
      x <- number
      return (Const x)
    neg = do 
      space
      char '-'
      char '('
      x <- liftM read $ PComb.some digit
      ctrl ")"
      return (Uminus (Const x))

parseAtom = (do 
    ctrl "("
    r <- parseExpression
    ctrl ")"
    return r
  ) <|> parseConstant

parseUminus = (do
    space
    char '-'
    char '('
    exp <- parseExpression
    ctrl ")"
    return (Uminus exp)
  ) <|> parseAtom

parseBinop :: Parser Exp -> [(String, Binop)] -> Parser Exp
parseBinop sub binops = do
    a <- sub
    bs <- PComb.many $ foldl (<|>) (do ctrl (fst (head binops)); s <- sub; return (snd (head binops), s)) [(do ctrl str; s <- sub; return (binop, s)) | (str, binop) <- drop 1 binops]
    when (bs == []) $ empty
    return (foldl (\a (binop, b) -> Binop binop a b) a bs)

parseProduct = parseBinop parseUminus [("*", Times), ("/", Div)] <|> parseUminus
parseTerm = parseBinop parseProduct [("+", Plus), ("-", Minus)] <|> parseProduct 
parseComparison = parseBinop parseTerm [("<", Less), ("<=", LessEq), ("==", Equal)] <|> parseTerm
parseAnd = parseBinop parseComparison [("&&", And)] <|> parseComparison
parseOr = parseBinop parseAnd [("||", Or)] <|> parseAnd




parseExpression = parseOr

parseExp :: String -> Maybe Exp
parseExp = parseAll parseExpression
  

command :: Parser Com
command = parseSeq <|> parseWhile <|> parseIf <|> parseAssignment

parseSeq :: Parser Com
parseSeq = (do
  ctrl "{"
  commands <- PComb.many (do x <- command; ctrl ";"; return x)
  lcommand <- command
  ctrl "}"
  return (Seq (commands ++ [lcommand]))
  ) <|> (do
    ctrl "{"
    ctrl "}"
    return (Seq [])
  )

parseAssignment :: Parser Com
parseAssignment = do
  name <- ident
  ctrl "="
  exp <- parseExpression
  return (Assign name exp)

parseWhile :: Parser Com
parseWhile = do
  keyword "while"
  exp <- parseExpression
  keyword "do"
  cmd <- command
  return (While exp cmd)

parseIf :: Parser Com
parseIf = do
  keyword "if"
  exp <- parseExpression
  keyword "then"
  cmd1 <- command
  keyword "else"
  cmd2 <- command
  return (If exp cmd1 cmd2)

parseCom :: String -> Maybe Com
parseCom = parseAll command   


printP = parse command . prettyCom
printE = parse parseExpression . prettyExp
