type ast =
    Const of int
  | Hex of int
  | Add of ast * ast 
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Neg of ast
  

