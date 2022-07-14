open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

let lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_expr env e =
  match e with
  | EConstInt i ->
      VInt i
  | EConstBool b ->
      VBool b
  | EVar x ->
      (try
         lookup x env
       with
       | Unbound -> raise EvalErr)
  | EAdd (e1,e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> VInt (i1 + i2)
       | _ -> raise EvalErr)
  | ESub (e1,e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        (match v1, v2 with
         | VInt i1, VInt i2 -> VInt (i1 * i2)
         | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        (match v1, v2 with
        | VInt i1, VInt i2 -> VInt (i1 / i2)
        | _ -> raise EvalErr)
  | EEq (e1,e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1, v2 with
       | VInt i1,  VInt i2  -> VBool (i1 = i2)
       | _ -> raise EvalErr)
  | ELt (e1,e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1, v2 with
       | VInt i1,  VInt i2  -> VBool (i1 < i2)
       | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
      let v1 = eval_expr env e1 in
      (match v1 with
       | VBool b ->
           if b then eval_expr env e2 else eval_expr env e3
       | _ -> raise EvalErr)
  |ELet (n,e1,e2) -> 
    eval_expr (extend n (eval_expr env e1) env) e2
  |EAnd (b1,b2) ->
    if (eval_expr env b1 = VBool(true)) && (eval_expr env b2 = VBool(true))
      then VBool(true) else VBool(false)
  |EOr (b1,b2) ->
    if (eval_expr env b1 = VBool(true)) || (eval_expr env b2 = VBool(true))
      then VBool(true) else VBool(false)
  |ENot(b) ->
    if (eval_expr env b = VBool(true))
      then VBool(false) else VBool(true)
     

let rec eval_cdecl1 l env (CDecl1(name,expr,command)) =
  let nvar = eval_expr env expr in
  let nenv = extend name nvar env in
  match command with
  | CDecl(n,exp) -> 
    let nnvar =  eval_expr nenv exp in
    (l@[("val "^name,nvar);("val "^n,nnvar)],extend n nnvar nenv)
  | CDecl1(n,exp,ncom) ->
    eval_cdecl1 (l@[("val "^name,nvar)])  nenv (CDecl1(n,exp,ncom))
  | _ -> assert false(*絶対に到達しない状態*)

let eval_command env c =(*出力する変数名と値の組のリストと環境の組を返す*)
  match c with
  | CExp e -> ([("-", eval_expr env e)], env)
  | CDecl (name ,expr) -> ([("val "^name,eval_expr env expr)],extend name (eval_expr env expr) env)
  | CDecl1(name,expr,command) -> 
    eval_cdecl1 [] env (CDecl1(name,expr,command))

