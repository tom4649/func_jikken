open TySyntax
open ConstraintSolver
(*let ty = TyFun(TyInt, TyInt)
let () =
  print_type ty;
  print_endline ""
  *)

  let a = TyVar (new_tyvar())
  let b = TyVar (new_tyvar())
  let c = TyVar(new_tyvar())
  let d = TyVar(new_tyvar())
  let e = TyVar(new_tyvar())
  let cnst = [(b,TyFun(c,d));(a,TyFun(d,e))]

  let ans = ty_subst(unify(cnst))
  
  let () = print_type (ans e);  print_endline ""
