{-
  Implement a pretty-printer for expressions and commands.
  Also refer to the syntax description in Readme.md!

  Here are the rules. As they are in English text, they might be ambiguous, so
  refer to the given test-sets for disambiguation!
  Aspects not specified by the rules can be implemented as you like (As long as you can parse your own output in the second part of this exercise).

  Expressions:
    As a general rule, pretty-printing shall not drop any information in the AST.
    In particular, the association of binary operators shall still be visible, i.e.
    do print Plus a (Plus b c) as "a+(b+c)" (and NOT as "a+b+c")

    * Only print parenthesis if they are required. In particular:

      Don't print parenthesis if given by operator priority, e.g.
        a*b+c instead of (a*b)+c

      Don't print parenthesis if given by operator associativity. All operators associate to the left!
      E.g., print:
        a-b-c instead of (a-b)-c
        a+b+c instead of (a+b)+c

      Unary minus: regard the unary minus rules in Readme.md, and be careful to not confuse the ASTs
        for "Uminus (Const x)" and "Const (-x)", e.g.

        Uminus (Const 42) -> "-(42)"
        Const (-x) -> -42

  Commands:
  * Separate commands in sequential composition by ;, use { and } as delimiters

    * insert line breaks
        after: do, then, else, command in sequential composition
      * Exception: do not insert line-breaks between {,} and then,else,do
      * Exception: do not insert line break between else and if

    * increase level of indentation inside if and while. Use 2 spaces per level of indentation.

    * Delimit all Seq-blocks with {}, even if nested

    E.g.

    {}   (for Seq [])

    {
      {}
    }

    {
      x = 1
    }

    {
      x = 1;
      y = 2
    }

    while c>0 do {
      x=x*2;
      c=c-1
    }

    if b then
      x=1
    else {
      x=x*3;
      y=1
    }

    if b then {
      x=1
    } else if c then {
      x=2;
      b=1
    } else {
      x=3
    }

-}
module Pretty (prettyExp, prettyCom) where

import While
import Data.List

{- Add your import declarations here! -}
symbols :: [(Binop, [Char])]
symbols = [(Plus, "+"), (Minus, "-"), (Times, "*"), (Div, "/"), (Equal, "=="), (Less, "<"), (LessEq, "<="), (And, "&&"), (Or, "||")]

priorities :: [(Binop, Integer)]
priorities = [(Plus, 4), (Minus, 4), (Times, 5), (Div, 5), (Equal, 3), (Less, 3), (LessEq, 3), (And, 2), (Or, 1)]

getSymbol :: Binop -> [Char]
getSymbol op = maybe "" snd(find (\(x, _) -> x == op) symbols)

priority :: Binop -> Integer
priority op = maybe 0 snd(find (\(x, _) -> x == op) priorities)

data Direction = L | R | U deriving(Eq)

prettyExp' :: Maybe Binop -> Direction -> Exp -> String
prettyExp' _ _ (Const x) = show x
prettyExp' _ _ (Var x) = x

prettyExp' _ _ (Uminus (Binop binop a b)) = "-" ++ "(" ++ prettyExp' Nothing U (Binop binop a b) ++ ")"
prettyExp' _ _ (Uminus (Const a)) = "-" ++ "(" ++ show a ++ ")"
prettyExp' _ _ (Uminus (Var a)) = "-" ++ a
prettyExp' _ _ (Uminus (Uminus exp)) = "-" ++ "(" ++ prettyExp' Nothing U (Uminus exp) ++ ")"

prettyExp' prev direction (Binop binop a b) = if brackets then "(" ++ x ++ ")" else x
  where
    brackets = case prev of
      Nothing -> False
      Just p -> not ((direction == L && priority p == priority binop) || priority binop > priority p )
    x = prettyExp' (Just binop) L a ++ getSymbol binop ++ prettyExp' (Just binop) R b


a = Binop Equal (Const 1) (Binop Less (Binop Minus (Var "b") (Binop Div (Binop Less (Var "a") (Const 11)) (Uminus (Const 0)))) (Binop Minus (Const 1) (Binop Plus (Var "a") (Binop Less (Const 0) (Var "b")))))


prettyExp :: Exp -> String
prettyExp = prettyExp' Nothing U 

indent :: Int -> String
indent x = replicate (2 * x) ' '

leveledCmd (Seq a) level isSeqException isIfException= prettyCom' (Seq a) level isSeqException isIfException
leveledCmd (If exp t e) level isSeqException True= prettyCom' (If exp t e) level isSeqException True
leveledCmd cmd level isSeqException isIfException = prettyCom' cmd (level + 1) isSeqException isIfException

firstChar level = if level == 0 then "" else "\n"

prettyCom' :: Com -> Int -> Bool -> Bool -> String
prettyCom' (Assign name exp) level _ _ = firstChar level ++ indent level ++ name ++ " = " ++ prettyExp exp
prettyCom' (If exp t e) level _ isIfException = beginning ++ "if " ++ prettyExp exp ++ " then" ++ cmd1 ++ br t ++ "else" ++ cmd2
 where
   cmd1 = leveledCmd t level True False
   cmd2 = leveledCmd e level True True
   beginning = if isIfException then " " else firstChar level ++ indent level
   br (Seq _) = " "
   br _ ="\n" ++ indent level
prettyCom' (While exp cmd) level _ _ = firstChar level ++ indent level ++ "while " ++ prettyExp exp ++ " do" ++ leveledCmd cmd level True False


-- prettyCom' (Seq [x]) = prettyCom' x
prettyCom' (Seq []) level isSeqException _ = beginning ++ "{}" where beginning = if isSeqException then " " else indent level
prettyCom' (Seq (x:xs)) level isSeqException _ = beginning ++ prettyCom' x (level + 1) False False ++ rest xs ++ indent (level) ++ "}"
  where
    beginning | isSeqException = " {"
              | level == 0 = "{"
              | otherwise = "\n" ++ indent level ++ "{"
    rest [] = "\n"
    rest (y:ys) = ";" ++ prettyCom' y (level + 1) False False ++ rest ys

prettyCom :: Com -> String
prettyCom c = prettyCom' c 0 False False

check ::  String -> String -> String -> IO()
check [] [] _ = putStrLn "DONE"
check (x:xs) (y:ys) a = if x == y || a == "{\n  while 0*z do\n    z = 1*x;\n  if 0 then {\n    x = y\n  } else\n    y = -9;\n  if " || a == "{\n  while 0*z do\n    z = 1*x;\n  if 0 then {\n    x = y\n  } else\n    y = -9;\n  if 0" then check xs ys (a ++ [x]) else putStrLn a

printCom = putStrLn . prettyCom
