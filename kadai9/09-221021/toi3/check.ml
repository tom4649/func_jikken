open ConstraintSolver
open Syntax
open Eval
let rec lookup x tenv =
  match tenv with
  | [] -> raise TyError
  | (a,t)::ll -> if x = a then t else lookup x ll

let rec pattern_to_const p =
  match p with
  | PInt _ -> (TyInt,[],[]) 
  | PBool _ -> (TyBool,[],[]) 
  | PVar x -> let a =TyVar(new_tyvar()) in (a,[],[(x,([],a))])
  |PPair(p1,p2) ->let (t1,c1,g1)=pattern_to_const p1 in let (t2,c2,g2)=pattern_to_const p2 in
  (TyPair(t1,t2),c1@c2,g1@g2)
  |PNil -> (Tylist(TyVar(new_tyvar())),[],[])
  |PCons(p1,p2)-> let (t1,c1,g1)=pattern_to_const p1 in let (t2,c2,g2)=pattern_to_const p2 in
  (t2,(Tylist(t1),t2)::c1@c2,g1@g2)
  let rec infer_expr tenv exp =
    match exp with
    | EConstInt _ -> (TyInt,[])
    | EConstBool _ -> (TyBool,[])
    | EVar x -> let shc = lookup x tenv in (instantiate shc,[]) 
    | EAdd (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | ESub (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | EMul (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | EDiv (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | EEq  (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | ELt  (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
    | EIf  (e1,e2,e3) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in let (t3,c3)=infer_expr tenv e3 in (t2,[(t1,TyBool);(t2,t3)]@c1@c2@c3)
    | ELet (x,e1,e2) -> 
      let (t1,c1) = infer_expr tenv e1 in 
      let sigma= unify c1 in let s1= ty_subst sigma t1 in  let nenv = env_subst sigma tenv in 
      let sch = generalize nenv s1 in let (t2,c2) = infer_expr ((x,sch)::nenv) e2 in (t2,c1@c2)
    | EFun (x,e) -> let a = TyVar(new_tyvar()) in  let tenv' = extend x ([],a) tenv in let (t,c) = infer_expr tenv' e in (TyFun(a,t),c)
    | EApp (e1,e2) -> let (t1,c1)=infer_expr tenv e1 in let (t2,c2)=infer_expr tenv e2 in let a = TyVar(new_tyvar()) in (a,(t1,TyFun(t2,a))::c1@c2)
    | ELetRec (f,x,e1,e2) -> let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
     let (t1,c1)=infer_expr ( (f,([],TyFun(a,b)))::(x ,([],a))::tenv ) e1 in
     let sigma= unify ((b,t1)::c1) in let s1= ty_subst sigma (TyFun(a,b)) in let nenv = env_subst sigma tenv in
     let sch = generalize nenv s1 in let (t2,c2) = infer_expr ((f,sch)::tenv) e2 in (t2,(b,t1)::c1@c2)
     |EPair(e1,e2) -> let (t1,c1) =infer_expr tenv e1 in let (t2,c2) =infer_expr tenv e2 in (TyPair(t1,t2),c1@c2)
     |ENil -> let x = new_tyvar() in (Tylist (TyVar x),[])
     |ECons(e1,e2)->let (t1,c1) =infer_expr tenv e1 in let (t2,c2) =infer_expr tenv e2 in (t2,(Tylist t1,t2)::c1@c2)
     |EMatch(e,l) ->
      let (t,c) = infer_expr tenv e in
      let a = TyVar(new_tyvar()) in
      let rec ematch_help ll ans=
      match ll with
      | [] -> ans
      | (p,ei)::lll ->let (ti,ci,gi)=pattern_to_const p in let (ti',ci')=infer_expr (tenv@gi) ei in
      ematch_help lll ((t,ti)::((a,ti')::ci@ci'@ans)) in
      (a,ematch_help l c)

  

let infer_cmd cmd tenv =
  match cmd with
  | CExp exp -> let (t,c)=infer_expr tenv exp in let t' = ty_subst (unify c) t in (t',tenv)
  | CDecl (x,exp) -> 
    let (t1,c1) = infer_expr tenv exp in 
    let sigma= unify c1 in let s1= ty_subst sigma t1 in let nenv = env_subst sigma tenv in
    let sch = generalize nenv s1 in (s1,(x,sch)::tenv)
  | CRecDecl (f,x,exp) -> 
    let a = TyVar(new_tyvar()) in let b = TyVar(new_tyvar()) in 
   let (t1,c1)=infer_expr ( (f,([],TyFun(a,b)))::(x ,([],a))::tenv ) exp in
   let sigma= unify ((b,t1)::c1) in let s1= ty_subst sigma (TyFun(a,b)) in let nenv = env_subst sigma tenv in
   let sch = generalize nenv s1 in (s1,(f,sch)::tenv)