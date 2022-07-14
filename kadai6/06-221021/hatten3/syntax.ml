type name = string

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
  | EFun       of name list* expr
  | EDFun       of name list* expr
  | EApp       of expr * expr
  | ELetRec    of (name * name * expr) list * expr

type command =
  | CExp     of expr
  | CDecl    of name * expr
  | CRecDecl of (name * name * expr) list

let print_name = print_string

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name*expr*env
  | VDFun of name*expr
  | VRecFun of int*((name*name*expr) list)*env
and 
env = (name * value) list
let print_value v =
    match v with
    | VInt i  -> print_int i
    | VBool b -> print_string (string_of_bool b)
    | VFun (_,_,_)-> print_string "<fun>"
    | VDFun (_,_)-> print_string "<fun>"
    | VRecFun (_,_,_)-> print_string "<fun>"

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
  | EFun (l,e) ->
      print_string ("EFun (" );
      List.iter (fun x -> print_string(x^",")) l;
      print_expr e;
      print_string ")"
  |EDFun(l,e) ->
    print_string ("EDFun (" );
    List.iter (fun x -> print_string(x^",")) l;
    print_expr e;
    print_string ")"
    | EApp (e1,e2) ->
      print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELetRec (decls,e) ->
      print_string ("ELetRec ([");
      List.iter (fun (id,x,e) ->
		 print_string ("(" ^ id ^ "," ^ x ^ ",");
		 print_expr e;
		 print_string ");")
		decls;
      print_string "],";
      print_expr e;
      print_string ")"

let print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (x,e) ->
      print_string ("CDecl (" ^ x ^ ",");
      print_expr e;
      print_string ")"
  | CRecDecl decls ->
      print_string ("ERecDecl ([");
      List.iter (fun (id,x,e) ->
		 print_string ("(" ^ id ^ "," ^ x ^ ",");
		 print_expr e;
		 print_string ");")
		decls;
      print_string "])"
