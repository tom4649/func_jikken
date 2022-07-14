type expr =
  | EConstInt of int
  | EAdd of expr*expr
  | ESub of expr*expr
  | EMul of expr*expr
  | EDiv of expr*expr
  | EConstBool of bool
  | EEqual of expr*expr
  | ELT of expr*expr
  | EIF of expr*expr*expr;;
