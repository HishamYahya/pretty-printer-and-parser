module While where

type Name = String
type Val = Integer

data Binop =
            Plus  | Minus | Times | Div
          | Equal | Less | LessEq
          | And | Or
  deriving (Show,Eq)

data Exp =
    Var Name
  | Const Val
  | Uminus Exp
  | Binop Binop Exp Exp
  deriving (Show,Eq)

data Com =
      Assign Name Exp
    | Seq [Com]
    | If Exp Com Com
    | While Exp Com
  deriving (Show,Eq)


