open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

let lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_Rec i l env env'=(*eval_exprにおいて再帰関数の表現を評価するためのヘルパー関数*)
  let n = List.length l in
  if i=n then env' else
    let (f,_,_) = List.nth l i in
    let env''= extend f (VRecFun(i,l,env)) env' in
    eval_Rec (i+1) l env env''


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
  |EFun (l,e)-> 
    (match l with
    | [x] -> VFun(x,e,env)
    | x::ll -> VFun(x,EFun(ll,e),env)
    | [] -> assert false (*絶対に到達しない状態*) )
  |EDFun (l,e)-> 
    (match l with
    | [x] -> VDFun(x,e)
    | x::ll -> VDFun(x,EDFun(ll,e))
    | [] -> assert false (*絶対に到達しない状態*) )
  |EApp (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | VFun(x,e,env') -> eval_expr (extend x v2 env') e
    | VDFun(x,e) -> eval_expr (extend x v2 env) e
    | VRecFun(i,l,oenv) ->
      let env' = eval_Rec 0 l oenv oenv in
      let (_,x,e) = List.nth l i in
      let env'' = extend x v2 env' in
      eval_expr env'' e
    | _ -> raise EvalErr)
  |ELetRec(l,e2) ->
    let env' = eval_Rec 0 l env env in
    eval_expr env' e2


let rec eval_CRecDecl i l env env' ans =(*CRecDeclを評価するための補助関数*)
  let n = List.length l in
  if i=n then (ans,env') else
    let (f,_,_) = List.nth l i in
    let env''= extend f (VRecFun(i,l,env)) env' in
    let ans'=ans@["val "^f,VRecFun(i,l,env)] in
    eval_CRecDecl (i+1) l env env'' ans'


let eval_command env c =(*返り値は、「出力のための文字列と値の組のリスト」と環境の組*)
  match c with
  | CExp e -> ([("-", eval_expr env e)], env)
  | CDecl (name ,expr) -> ([("val "^name,eval_expr env expr)],extend name (eval_expr env expr) env)
  | CRecDecl l -> eval_CRecDecl 0 l env env []