open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

let lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

type 'a option = None | Some of 'a
let rec find_match p v =
  match p with
  | PInt n -> (match v with | VInt m -> if n = m then Some [] else None |_ -> None)
  | PBool b -> (match v with | VBool c -> if b = c then Some [] else None |_ -> None)
  | PVar x -> (if x = "_" then Some [] else Some[(x,v)])
  | PPair(p1,p2) ->(match v with 
  | VPair(v1,v2) -> 
    let l1 = find_match p1 v1 in let l2= find_match p2 v2 in 
  (match l1 with
  | Some a1 -> (match l2 with | Some a2 -> Some (a1@a2) | _ -> None)
  | _ -> None)
  | _ -> None)
  | PNil -> (match v with | VNil -> Some [] | _ -> None)
  | PCons (p1,p2) -> (match v with 
  | VCons(v1,v2) -> 
    let l1 = find_match p1 v1 in let l2= find_match p2 v2 in 
  (match l1 with
  | Some a1 -> (match l2 with | Some a2 -> Some (a1@a2) | _ -> None)
  | _ -> None)
  | _ -> None)

let rec eval_expr env e =
  (*print_expr e;print_string"\n";(*評価順序の確認のための出力*)*)
  match e with
  | EConstInt i ->
      VInt i
  | EConstBool b ->
      VBool b
  | EVar x ->
      (try
         let r=lookup x env in
         (match !r with
         |DVal v -> v
         |DThunk (e0,env') -> 
          let v = eval_expr env' e0 in
          r:= DVal v ; v)
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
    eval_expr env (EApp(EFun(n,e2),e1))
  |EFun (x,e)-> VFun(x,e,env)
  |EApp (e1,e2) ->
    let v1 = eval_expr env e1 in
    (match v1 with
    | VFun(x,e,env') -> 
      let r = ref (DThunk(e2,env))in
      eval_expr ((x,r)::env') e
    | VRecFun(f,x,e,oenv) ->
      let rec oenv'=(f,ref (DThunk(EFun(x,e),oenv')))::oenv in
      let r = ref (DThunk(e2,env))in      
      let env' = (x,r)::oenv' in
      eval_expr env' e
    | _ -> raise EvalErr)
    |ELetRec(f,x,e1,e2) ->
      let rec env' = ((f,ref (DThunk(EFun(x,e1),env'))))::env in
      eval_expr env' (EApp(EFun(f,e2),EVar(f)))
    |EPair(e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VPair(v1,v2)
  |ENil -> VNil
  |ECons(e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VCons(v1,v2)
  (*|EMatch(e,l) ->
    let v = eval_expr env e in
    let rec ematch_help ll = (match ll with
    | (p,ei)::lll -> let res = find_match p v in
    (match res with
    | Some li -> eval_expr (li@env) ei
    | None -> ematch_help lll)
    | _ -> raise EvalErr) in
    ematch_help l*)

let eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (name ,expr) -> 
    let r = ref (DThunk(expr,env)) in
    ("val "^name,(name,r)::env,eval_expr env expr)
  | CRecDecl (f,x,e) -> 
    let rec env' = (f,ref (DThunk(EFun(x,e),env')))::env in
    ("val "^f,env',VRecFun(f,x,e,env))