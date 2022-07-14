type name = string
let print_pair_flag_value =0(*ペアの出力を切り替えるフラグ*)

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
  | EFun       of name * expr
  | EApp       of expr * expr
  | ELetRec    of name * name * expr * expr
  | EPair of expr*expr

type command =
  | CExp     of expr
  | CDecl    of name * expr
  | CRecDecl of name * name * expr

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name*expr*env
  | VRecFun of name*name*expr*env
  | VPair of value*value
and 
env = (name * value) list


let print_name = print_string

let rec print_value v =
    match v with
    | VInt i  -> print_int i
    | VBool b -> print_string (string_of_bool b)
    | VFun (_,_,_)-> print_string "<fun>"
    | VRecFun (_,_,_,_)-> print_string "<fun>"
    | VPair(v1,v2) -> 
        if print_pair_flag_value=0 then (print_string "<pair>")(*この課題の入力の場合、出力が終わらないので省略*)
    else (print_string"("; print_value v1;print_string ",";print_value v2;print_string")")
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
  | ELet (x,e1,e2) ->
      print_string ("ELet (" ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EFun (x,e) ->
      print_string ("EFun (" ^ x ^ ",");
      print_expr e;
      print_string ")"
  | EApp (e1,e2) ->
      print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELetRec (id,x,e1,e2) ->
      print_string ("ELetRec (" ^ id ^ "," ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  |EPair (e1,e2) ->print_string "EPair (";
  print_expr e1;
  print_string ",";
  print_expr e2;
  print_string ")"

let  print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (x,e) ->
      print_string ("CDecl (" ^ x ^ ",");
      print_expr e;
      print_string ")"
  | CRecDecl (id,x,e) ->
      print_string ("CRecDecl (" ^ id ^ "," ^ x ^ ",");
      print_expr e;
      print_string ")"
