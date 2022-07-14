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
  |EFun (x,e)-> VFun(x,e,env)
  |EApp (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | VFun(x,e,env') -> eval_expr (extend x v2 env') e
    | VRecFun(f,x,e,oenv) ->
      let env' = (extend x v2 (extend f (VRecFun(f,x,e,oenv))oenv)) in
      eval_expr env' e
    | _ -> raise EvalErr)
  |ELetRec(f,x,e1,e2) ->
    let env' = extend f (VRecFun(f,x,e1,env )) env in
    eval_expr env' e2
  |EPair(e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VPair(v1,v2)
  |ENil -> VNil
  |ECons(e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VCons(v1,v2)



let eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (name ,expr) -> ("val "^name,extend name (eval_expr env expr) env,eval_expr env expr)
  | CRecDecl (f,x,e) -> 
    ("val "^f,extend f (VRecFun(f,x,e,env)) env,VRecFun(f,x,e,env))