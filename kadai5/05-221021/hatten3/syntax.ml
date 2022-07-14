type name = string

type value =
  | VInt  of int
  | VBool of bool

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EAnd       of expr * expr
  | EOr       of expr * expr
  | ENot       of expr

type command =
  | CExp  of expr
  | CDecl of name * expr
  | CDecl1 of name*expr*command
  | CLetAnd of name*expr*command
  |CLet of name*expr*expr
let print_name = print_string

let print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   https://ocaml.org/api/Format.html
 を活用すること
*)
let rec print_expr e =
  match e with
  | EConstInt i ->
      print_int i
  | EConstBool b ->
      print_string (string_of_bool b)
  | EVar x ->
      print_name x
  | EAdd (e1,e2) ->
      print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ESub (e1,e2) ->
      print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EMul (e1,e2) ->
      print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EDiv (e1,e2) ->
      print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EEq (e1,e2) ->
      print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELt (e1, e2) ->
      print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EIf (e1,e2,e3) ->
      print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")"
  | ELet (n,e1,e2) ->
        print_string "ELet (";
        print_string   n;
        print_string ",";
        print_expr   e1;
        print_string ",";
        print_expr   e2;
        print_string ")"
  |EAnd (b1,b2) ->
    print_string "EAnd (";
    print_expr b1;
    print_string ",";
    print_expr b2;
    print_string ")"
  |EOr (b1,b2) ->
      print_string "EOr (";
      print_expr b1;
      print_string ",";
      print_expr b2;
      print_string ")"
  |ENot (b) ->
        print_string "ENot (";
        print_expr b;
        print_string ")"
    
  
  


let rec print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (n,e) -> print_string "CDecl ("; 
                  print_name n;
                  print_string ",";
                  print_expr e;
                  print_string ")"

  | CDecl1 (n,e,c) -> print_string "CDecl1 "; 
                  print_name n;
                  print_string ",";
                  print_expr e;
                  print_newline();
                  print_command c;
                  print_string ")"
  | CLetAnd (n,e,c) -> print_string "CLetAnd "; 
                  print_name n;
                  print_string ",";
                  print_expr e;
                  print_newline();
                  print_command c;
                  print_string ")"

  | CLet (n,e1,e2) -> 
    print_string "CLet (";
    print_string   n;
    print_string ",";
    print_expr   e1;
    print_string ",";
    print_expr   e2;
    print_string ")"


      



