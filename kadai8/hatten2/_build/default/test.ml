open ConstraintSolver
open Syntax
open Eval
open Check

let num = ref 0


let rec test tenv exp =
  match exp with
  | EConstInt _ -> (TyInt,[])
  | EConstBool _ -> (TyBool,[])
  | EVar x -> let shc = lookup x tenv in (instantiate shc,[]) 
  | EAdd (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | ESub (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EMul (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EDiv (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EEq  (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | ELt  (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EIf  (e1,e2,e3) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in let (t3,c3)=test tenv e3 in (t2,[(t1,TyBool);(t2,t3)]@c1@c2@c3)
  | ELet (x,e1,e2) -> Printf.printf "let";flush stdout;
    let (t1,c1) = test tenv e1 in let n = !num in num:=n+1;Printf.printf "%d" n; flush stdout;
    let sigma= unify c1 in Printf.printf "unified"; flush stdout;let s1= ty_subst sigma t1 in Printf.printf "substed";flush stdout; let nenv = env_subst sigma tenv in Printf.printf "envsubsted";flush stdout;
    let sch = generalize nenv s1 in Printf.printf "generalized\n";flush stdout; let (t2,c2) = test ((x,sch)::nenv) e2 in (t2,c1@c2)
  | EFun (x,e) -> let a = TyVar(new_tyvar()) in  let tenv' = extend x ([],a) tenv in let (t,c) = test tenv' e in (TyFun(a,t),c)
  | EApp (e1,e2) -> let (t1,c1)=test tenv e1 in let (t2,c2)=test tenv e2 in let a = TyVar(new_tyvar()) in (a,(t1,TyFun(t2,a))::c1@c2)
  | ELetRec (f,x,e1,e2) -> let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
   let (t1,c1)=test ( (f,([],TyFun(a,b)))::(x ,([],a))::tenv ) e1 in
   let sigma= unify ((b,t1)::c1) in let s1= ty_subst sigma (TyFun(a,b)) in let nenv = env_subst sigma tenv in
   let sch = generalize nenv s1 in let (t2,c2) = test ((f,sch)::tenv) e2 in (t2,(b,t1)::c1@c2)
   |EPair(e1,e2) -> let (t1,c1) =infer_expr tenv e1 in let (t2,c2) =infer_expr tenv e2 in if t1=t2 then let x = get_pair t1 in (TyPair(x,x),c1@c2) else (TyPair(get_pair t1,get_pair t2),c1@c2)

  let infer_test cmd tenv =
    match cmd with
    | CExp exp -> let (t,c)=test tenv exp in let t' = ty_subst (unify c) t in (t',tenv)
    | CDecl (x,exp) -> 
      let (t1,c1) = test tenv exp in 
      let sigma= unify c1 in let s1= ty_subst sigma t1 in let nenv = env_subst sigma tenv in
      let sch = generalize nenv s1 in (s1,(x,sch)::tenv)
    | CRecDecl (f,x,exp) -> 
      let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
     let (t1,c1)=test ( (f,([],TyFun(a,b)))::(x ,([],a))::tenv ) exp in
     let sigma= unify ((b,t1)::c1) in let s1= ty_subst sigma (TyFun(a,b)) in let nenv = env_subst sigma tenv in
     let sch = generalize nenv s1 in (s1,(f,sch)::tenv)

(*
let x = fun x -> (x,x) in
let x = fun y -> x(x y) in
let x = fun y -> x(x y) in
let x = fun y -> x(x y) in
let x = fun y -> x(x y) in
let x = fun y -> x(x y) in
x(fun x->x)
*)