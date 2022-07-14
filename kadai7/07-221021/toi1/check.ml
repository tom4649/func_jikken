open ConstraintSolver
open TySyntax
open Syntax
open Eval

type tyenv = (name*ty)list
let rec lookup x tenv =
  match tenv with
  | [] -> raise TyError
  | (a,t)::ll -> if x = a then t else lookup x ll
let rec infer_expr tenv exp =
  match exp with
  | EConstInt _ -> (TyInt,[])
  | EConstBool _ -> (TyBool,[])
  | EVar x -> let t = lookup x tenv in (t,[])
  | EAdd (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | ESub (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EMul (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EDiv (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EEq  (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | ELt  (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  | EIf  (e1,e2,e3) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in let (t3,c3)=infer_expr tenv e3 in (t2,[(t1,TyBool);(t2,t3)]@c1@c2@c3)
  | ELet (x,e1,e2) -> let (t1,c1) = infer_expr tenv e1 in let tenv' = extend x t1 tenv in let (t2,c2) = infer_expr tenv' e2 in (t2,c1@c2)
  | EFun (x,e) -> let a = TyVar(new_tyvar()) in let tenv' = extend x a tenv in let (t,c) = infer_expr tenv' e in (TyFun(a,t),c)
  | EApp (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in let a = TyVar(new_tyvar()) in (a,(t1,TyFun(t2,a))::c1@c2)
  | ELetRec (f,x,e1,e2) -> let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
  let tenv' = extend f (TyFun(a,b)) tenv in let tenv'' = extend x a tenv' in let(t1,c1) = infer_expr tenv'' e1 in let (t2,c2) = infer_expr tenv' e2 
in (t2,(t1,b)::c1@c2)
let infer_cmd cmd tenv =
  match cmd with
  | CExp exp -> let (t,c)=infer_expr tenv exp in let t' = ty_subst (unify c) t in (t',tenv)
  | CDecl (x,exp) -> let (t,c) = infer_expr tenv exp in let t' = ty_subst (unify c) t in let tenv' = extend x t' tenv in (t',tenv') 
  | CRecDecl (f,x,exp) -> let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
  let tenv' = extend f (TyFun(a,b)) tenv in let tenv'' = extend x a tenv' in let(t1,c1) = infer_expr tenv'' exp in 
  let t' = ty_subst (unify ((t1,b)::c1)) (TyFun(a,b)) in let tenv''' = extend f t' tenv in (t',tenv''')