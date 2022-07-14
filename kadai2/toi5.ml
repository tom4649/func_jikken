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

type value =
  VInt of int | VBool of bool

exception Eval_error;;
let rec eval tar =
  match tar with
  | EConstInt(n) -> VInt(n)
  | EAdd(n,m) -> (match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> VInt(a+b) 
                                                                  | _ -> raise Eval_error)
                                    |_ -> raise Eval_error)
  | ESub(n,m) -> (match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> VInt(a-b) 
                                                                  | _ -> raise Eval_error)
                                    |_ -> raise Eval_error)
  | EMul(n,m) -> (match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> VInt(a*b) 
                                                                  | _ -> raise Eval_error)
                                    |_ -> raise Eval_error)                                    
  | EDiv(n,m) ->( match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> if b = 0 then raise Division_by_zero else VInt(a/b) 
                                                                  | _ -> raise Eval_error)
                                    |_ -> raise Eval_error)
  | EConstBool(x) -> VBool(x)
  | EEqual(n,m) -> (match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> if a=b then VBool(true) else VBool(false) 
                                                                    | _ -> raise Eval_error )
                                |_ -> raise Eval_error )
  | ELT(n,m) -> (match eval(n) with VInt(a) -> (match eval(m) with VInt(b) -> if a<b then VBool(true) else VBool(false) 
                                                                    | _ -> raise Eval_error )
                                |_ -> raise Eval_error)
  | EIF(l,n,m) -> (match eval(l) with VBool(x) -> if x then eval(n) else eval(m)
                                |_ -> raise Eval_error );; 

(*(*以下テスト用コード*)
let exp1 = EMul(EAdd(EConstInt(3),EConstInt(7)),EDiv(EConstInt(4),EConstInt(2)));;
let exp2 = EEqual(EConstInt(3),EConstInt(5));;
let exp3 = ELT(EConstInt(3),EConstInt(5));;
let exp4 = EIF(exp3,exp1,exp2);;
let exp5 = ELT(EConstBool(true),EConstInt(4));;
let exp6 = EDiv(EConstInt(4),EConstInt(0));;

eval exp1;;
*)